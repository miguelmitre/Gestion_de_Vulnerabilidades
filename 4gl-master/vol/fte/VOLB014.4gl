#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => EFP                                           #
#Programa          => VOLB014                                       #
#Descripcion       => MANTENEDOR DE APORTACIONES VOLUNTARIAS        #
#                     CON REDES COMERCIALES                         #
#Sistema           => VOL                                           #
#Fecha creacion    => 29 ABRIL 2014                                 #
#Po                => JOSE LUIS NEGRETE JUAREZ                      #
#####################################################################
#------------------------------------------------------------------------------#
# Ultima Act.      => 28MARZO2017    INV-4456                                  #
#                    	Se Adecuara el programa que carga el archivo de la       #
#                     dispersión de Redes Comerciales,considerando el layout   #
#                     0394XX adjunto:                                          #
#                                                                              #
#                      •Se agrega el ID 22 Tipo de ahorrador en el detalle 02. #
#                        En el cual PROCESAR indicará si corresponde a 01      #
#                        “Registro” o bien, “02 Registro Móvil”. Este dato     #
#                        únicamente se guardará como parte de los datos        #
#                        dispersados en el archivo.                            #
#                                                                              #
#                      •El ID 23 correspondiente a “Filler”, sólo cambia de    #
#                      longitud                                                #
#                                                                              #
#                     b)	El programa utilizado para el reverso de la carga del#
#                        archivo, también deberá considerar los nuevos campos. #
#                                                                              #
#  Archivo debe estar en la Ruta => "/safre_prc/vol/rescate"                   #                                                                          #
#  Archivo Mod sube_voldet_rc  /safre/vol/exp                                  #
###############################################################################

DATABASE  safre_af

GLOBALS
   DEFINE
      enter            CHAR(01),
      G_LISTA          CHAR(100),
      lp               CHAR(100),
      permisos         CHAR(100),
      usuario          CHAR(40),
      v_arch           CHAR(60),
      vreporte         CHAR(200),
      vruta_ejecuta    CHAR(40),
      vruta_rescate    CHAR(40),
      vruta_listados   CHAR(40)

   DEFINE
      HOY               DATE

   DEFINE
      v_folio_cod      INTEGER

   DEFINE
      vtot_acep        ,
      vtot_rech        ,
      vtot_reg         SMALLINT

   DEFINE gar_cifras_prev ARRAY[500] OF RECORD #INV-5470
      subcuenta SMALLINT     ,
      siefore   SMALLINT     ,
      acciones  DECIMAL(22,6),
      precio    LIKE glo_valor_accion.precio_del_dia,
      pesos     DECIMAL(22,6)
   END RECORD
END GLOBALS


MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

      DEFER INTERRUPT

      CALL inicio()

      CALL STARTLOG (FGL_GETENV("USER")||".VOLB014.log")

      OPEN WINDOW v1 AT 2,3 WITH 3 ROWS , 76 COLUMNS
      ATTRIBUTE(BORDER)
           MENU "APORTACIONES VENTANILLA CON REDES COMERCIALES"
                COMMAND "Carga" "Carga Archivo Apor Vol Redes Comercial"
                        CALL principal()
                COMMAND "Consulta" "Consulta Apor Vol Redes Comercial"
                        CALL consultar()
                COMMAND "Liquidación" "Liquidación Apor Vol Redes Comercial"
                        CALL liquida()
                COMMAND "Reversa" "Reversos Apor Vol Redes Comercial"
                        CALL reverso()
                COMMAND "Salir" "Salir"
                        EXIT MENU
           END MENU
      CLOSE WINDOW v1

END MAIN


FUNCTION inicio()
#----------------

   LET HOY  = TODAY

   SELECT ruta_exp,
          ruta_rescate,
          ruta_listados,
          USER
   INTO   vruta_ejecuta,
          vruta_rescate,
          vruta_listados,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "vol"

   LET G_LISTA = vruta_listados CLIPPED,"/",HOY  USING "DDMMYYYY",
                 ".VOLB014"

END FUNCTION


FUNCTION principal()
#-------------------

   DEFINE
      vpausa           CHAR(1),
      ejecuta          CHAR(200)

   DEFINE
      v_folio_cod      INTEGER

   DEFINE
      ls_result        SMALLINT

   DEFINE #decimal
      ld_tot_detalle           ,
      ld_tot_sum               ,
      ld_tot_cpp               DECIMAL(16,2)


   INITIALIZE v_arch TO NULL
   LET ls_result = TRUE

   LET HOY  = TODAY

   LET ejecuta = "cd ",vruta_rescate CLIPPED,"; ls > archivos_vol" CLIPPED
   RUN ejecuta

   CREATE TEMP TABLE archivos_vol (lista  CHAR(100))

   LET ejecuta = vruta_rescate CLIPPED,"/archivos_vol" CLIPPED

   LOAD FROM ejecuta INSERT INTO archivos_vol

   LET ejecuta = "rm ",vruta_rescate CLIPPED,"/archivos_vol" CLIPPED
   RUN ejecuta

   OPEN WINDOW v2 AT 5,3 WITH FORM "VOLB0141" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " VOLB014         APORTACIONES VOLUNTARIAS CON REDES COMERCIALES             " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY   USING"DD-MM-YYYY " AT 2,66 ATTRIBUTE(REVERSE)

   INPUT BY NAME v_arch WITHOUT DEFAULTS
      AFTER FIELD v_arch
         IF v_arch IS NULL THEN
            ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO "
            NEXT FIELD v_arch
         END  IF

         SELECT "x"
         FROM   archivos_vol
         WHERE  lista = v_arch

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE "
            NEXT FIELD v_arch
         END IF

      ON KEY (ESC)
         IF v_arch IS NULL THEN
            ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO "
            NEXT FIELD v_arch
         END IF

         SELECT "m.x"
         FROM   archivos_vol m
         WHERE  m.lista = v_arch

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE "
            NEXT FIELD v_arch
         END IF

         SELECT "b.X"
         FROM   int_archivo_vol b
         WHERE  b.nombre_archivo = v_arch

         IF SQLCA.SQLCODE = 0 THEN
            ERROR " NOMBRE DE ARCHIVO YA CARGADO "
            NEXT FIELD v_arch
         ELSE
            --CALL valida_archivo() # TODO Falta definir los campos obligatorios del layout

            INSERT INTO int_archivo_vol
            VALUES (v_arch,      -- nombre del archivo
                    HOY ,        -- fecha de carga del archivo
                    usuario,     -- usuario que carga el archivo
                    NULL         -- folio
                    )
         END IF

         DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)
         SLEEP 3

         CALL separa_archivo()

         CALL sube_datos()
            RETURNING v_folio_cod

         CALL actualiza_registros(v_folio_cod)
               RETURNING ls_result, ld_tot_sum, ld_tot_detalle, ld_tot_cpp

         IF ls_result THEN # ACEPTADO
            SELECT COUNT(*)
            INTO   vtot_reg
            FROM   int_det_vol_rc
            WHERE  folio = v_folio_cod

            SELECT COUNT(*)
            INTO   vtot_acep
            FROM   int_det_vol_rc
            WHERE  folio = v_folio_cod
            AND    estado = 1           # Aceptados

            SELECT COUNT(*)
            INTO   vtot_rech
            FROM   int_det_vol_rc
            WHERE  folio = v_folio_cod
            AND    estado = 0           # Rechazados

            DISPLAY "TOTAL DE REGISTROS : ",vtot_reg AT 8,20
            DISPLAY "TOTAL ACEPTADOS    : ",vtot_acep AT 9,20
            DISPLAY "TOTAL RECHAZADOS   : ",vtot_rech AT 10,20

            DISPLAY "" AT 15,1
            ERROR ""

            PROMPT " PROCESO FINALIZADO,FOLIO ASIGNADO:",
                   v_folio_cod USING "<<<<<<<<<<<<", #INV-5470
                   " <ENTER> PARA SALIR " ATTRIBUTE (REVERSE)
                   FOR vpausa

            EXIT INPUT
         ELSE  # RECHAZADO
            ERROR ""
            DISPLAY "LOS MONTOS CARGADOS NO COINCIDEN " AT 10,7
            DISPLAY "CON LO REPORTADO EN EL SUMARIO DEL ARCHIVO " AT 11,7

            DISPLAY "TOTAL SUMARIO : ",ld_tot_sum     USING "###,###.##" AT 13,20
            DISPLAY "TOTAL CPP     : ",ld_tot_cpp     USING "###,###.##" AT 14,20
            DISPLAY "TOTAL CARGADO : ",ld_tot_detalle USING "###,###.##" AT 15,20

            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR enter
            EXIT INPUT
         END IF

      ON KEY (INTERRUPT, CONTROL-C)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR enter
         EXIT INPUT
   END INPUT

   LET ejecuta = "cd ",vruta_rescate CLIPPED,"/;",
                 "touch adi cza1 cza_vol.394 det1 det_vol_rc det_vol_rc_cpp ",
                 "sum1 sum_vol.394 vfolio_cza vfolio_det vfolio_det1 ",
                 "vfolio_det2 vfolio_det3 vfolio_sum; ",
                 "rm adi cza1 cza_vol.394 det1 det_vol_rc det_vol_rc_cpp ",
                 "sum1 sum_vol.394 vfolio_cza vfolio_det vfolio_det1 ",
                 "vfolio_det2 vfolio_det3 vfolio_sum "
   RUN ejecuta

   CLOSE WINDOW v2
   DROP TABLE archivos_vol
   RETURN
END FUNCTION


FUNCTION valida_archivo()
#------------------------

   DEFINE
      carga_reg   CHAR(260),
      comando     CHAR(150),
      afore_cod   CHAR(003)

   DEFINE
      cont_reg    INTEGER

   LET cont_reg = 0

   CALL crea_tablas()

   LET comando = vruta_rescate CLIPPED,"/",v_arch CLIPPED

   WHENEVER ERROR CONTINUE
      LOAD FROM comando INSERT INTO safre_tmp:arch_vol_rc
   WHENEVER ERROR STOP

   DECLARE cur_1 CURSOR FOR
   SELECT  *
   FROM    safre_tmp:arch_vol_rc

   FOREACH cur_1 INTO carga_reg

      IF carga_reg[001,002] = "01" THEN
         IF carga_reg[003,004] = "03" THEN  #-- Id servicio Recaudacion --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN ENCABEZADO: Id 2 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[005,006] = "94" THEN #-- Id Operación --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN ENCABEZADO: Id 3 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[007,008] = "03" THEN #-- Tipo Entidad Origen --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN ENCABEZADO: Id 4 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[009,011] = "001" THEN #-- Clave Entidad Origen --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN ENCABEZADO: Id 5 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[012,013] = "01" THEN #-- Tipo Entidad Destino --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN ENCABEZADO: Id 6 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         LET afore_cod = carga_reg[014,016]

         SELECT "OK"
         FROM   tab_afore
         WHERE  afore_cod = afore_cod

         IF SQLCA.SQLCODE = 0 THEN #-- Clave Entidad Destino --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN ENCABEZADO: Id 7 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[017,024] >= 0 THEN #-- Fecha Creación Lote --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN ENCABEZADO: Id 8 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[025,027] >= 0 THEN #-- Consecutivo día --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN ENCABEZADO: Id 9 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF
      END IF

      IF carga_reg[001,002] = "02" THEN
         LET cont_reg = cont_reg + 1

         IF carga_reg[003,009] >= 0 THEN #-- Consecutivo de Registros --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 2 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[010,020] <> "           " THEN #-- NSS --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 3 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[021,038] <> "                  " THEN #-- CURP --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 4 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[039,078] <> "                                      " THEN #-- Paterno --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 5 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[119,158] <> "                                      " THEN #-- Paterno --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 7 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[035,036] = "10" #-- Beneficio Fiscal --#
         OR carga_reg[035,036] = "20"
         OR carga_reg[035,036] = "30" THEN
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 7 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[037,038] = "90"
         OR carga_reg[037,038] = "10"
         OR carga_reg[037,038] = "20"
         OR carga_reg[037,038] = "30"
         OR carga_reg[037,038] = "40"
         OR carga_reg[037,038] = "50" THEN #-- Siefore --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 8 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[039,040] >= 0 THEN #-- Campos en cero --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 9 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[041,041] >= 0 THEN #-- Digito Verificador --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 10 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[084,091] >= 0 THEN #-- Fecha de captura  del formato  --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 12 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[092,099] >= 0 THEN #-- Fecha de presentación del pago --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 13 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[100,107] >= 0 THEN #-- Fecha de aplicación de recursos --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 14 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[108,122] >= 0 THEN #-- Importe del documento --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 15 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[124,124] >= 0 THEN #-- Importe de intereses o  descuentos  --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 17 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[138,152] >= 0 THEN #-- Importe neto del pago --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 19 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[171,185] >= 0 THEN #-- Importe pagado con doc. De cobro inmediato --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 22 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[189,203] >= 0 THEN #-- Importe pagado con documento de remesa --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 24 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[207,221] >= 0 THEN #-- Importe del documento sin IVA --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 26 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[222,232] >= 0 THEN #-- IVA de documento --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 27 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[233,236] >= 0 THEN #-- IVA de documento --#
            #-- PASA VALIDACION --#
         ELSE
            DISPLAY " ERROR EN DETALLE: Registro ",cont_reg,",Id 28 CON VALOR INVALIDO..." AT 15,1
            PROMPT  " <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF
      END IF

      IF carga_reg[001,001] = "T" THEN

         IF carga_reg[002,010] >= 0 THEN #-- Número Total de Movimientos Contenidos --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN SUMARIO: Id 2 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[011,027] >= 0 THEN #-- Importe Total Recaudado --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN SUMARIO: Id 3 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[028,044] >= 0 THEN #-- Importe Total IVA --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN SUMARIO: Id 4 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[045,061] >= 0 THEN #-- Importe Total Neto (sin iva) --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN SUMARIO: Id 5 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[062,072] >= 0 THEN #-- Importe Total de la Comisión por el Servicio --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN SUMARIO: Id 6 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF carga_reg[073,081] >= 0 THEN #-- Importe Total del IVA por la Comisión --#
            #-- PASA VALIDACION --#
         ELSE
            PROMPT " ERROR EN SUMARIO: Id 7 CON VALOR INVALIDO...<ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF
      END IF
   END FOREACH
END FUNCTION


FUNCTION crea_tablas()
#---------------------

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE arch_vol_rc
   WHENEVER ERROR STOP

   CREATE TABLE arch_vol_rc
      (
      n_registros   CHAR(260)
      )

   DATABASE safre_af

END FUNCTION


FUNCTION separa_archivo()
#------------------------

   DEFINE ejecuta            CHAR(200)

   ERROR " SEPARANDO ARCHIVO,ETAPA 2 "

   LET ejecuta = "sed -e '/^01/!d' ",vruta_rescate CLIPPED,"/",
                v_arch CLIPPED," >",vruta_rescate CLIPPED,"/cza_vol.394"
   RUN ejecuta

   LET ejecuta = "sed -e '/^02/!d' ",vruta_rescate CLIPPED,"/",
                v_arch CLIPPED," >",vruta_rescate CLIPPED,"/det_vol_rc"
   RUN ejecuta

   LET ejecuta = "sed -e '/^08/!d' ",vruta_rescate CLIPPED,"/",
                v_arch CLIPPED," >",vruta_rescate CLIPPED,"/det_vol_rc_cpp"
   RUN ejecuta

   LET ejecuta = "sed -e '/^09/!d' ",vruta_rescate CLIPPED,"/",
                v_arch CLIPPED," >",vruta_rescate CLIPPED,"/sum_vol.394"
   RUN ejecuta

   ERROR " SEPARACION DE ARCHIVO TERMINADO,ETAPA 2 "

END FUNCTION


FUNCTION sube_datos()
#--------------------

   DEFINE
      ejecuta            CHAR(200),
      vtipo_registro     CHAR(2),
      vcontenido         CHAR(1)

   DEFINE
      v_folio_cod        INTEGER

   LET v_folio_cod = 0

   INSERT INTO safre_af:glo_folio
   VALUES(0)

   SELECT MAX(folio)
   INTO   v_folio_cod
   FROM   safre_af:glo_folio

   UPDATE int_archivo_vol
   SET    folio = v_folio_cod
   WHERE  nombre_archivo = v_arch

   --------------------   GENERA adi   --------------------

   LET vreporte = vruta_rescate CLIPPED,"/adi"

   START REPORT salida TO vreporte
      LET vcontenido = "a"

      OUTPUT TO REPORT salida(vcontenido)
   FINISH REPORT salida


   --------------------   GENERA ENCABEZADO   --------------------

   LET vreporte = vruta_rescate CLIPPED,"/vfolio_cza"

   START REPORT salida1 TO vreporte
      LET vtipo_registro = "01"

      OUTPUT TO REPORT salida1(v_folio_cod,
                               vtipo_registro
                              )
   FINISH REPORT salida1

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; paste vfolio_cza cza_vol.394 > cza1" CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "/;sed 's/	//g' cza1 > cza_vol.394 "
   RUN ejecuta

   LET ejecuta = "cd ",vruta_ejecuta CLIPPED,
                 "/;DBDATE=y4md;export DBDATE;",
                 "dbload -d safre_af -c sube_volrc_cza_394 -l /tmp/",
                 usuario CLIPPED,".VOLRC_CZA.log;"
   RUN ejecuta

   --------------------   GENERA DETALLE   --------------------

   LET vreporte = vruta_rescate CLIPPED,"/vfolio_det"

   START REPORT salida1 TO vreporte
      LET vtipo_registro = "02"

      OUTPUT TO REPORT salida1(v_folio_cod,
                               vtipo_registro
                              )

   FINISH REPORT salida1

   -- Se crea archivo adi que contiene a\  y se pega en vfolio_det

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; cat adi vfolio_det > vfolio_det1" CLIPPED
   RUN ejecuta

   -- Se crea archivo vfolio_det con el numero de registros igual al
   -- num. registros que tiene el detalle

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; sed -f vfolio_det1 det_vol_rc > vfolio_det2" CLIPPED
   RUN ejecuta

   -- Se crea archivo vfolio_det borrando lineas que no sirven

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; sed -e '/^ /!d' vfolio_det2 > vfolio_det3" CLIPPED
   RUN ejecuta

   -- Se crea det1 pegando datos genrales con el detalle

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; paste vfolio_det3 det_vol_rc > det1" CLIPPED
   RUN ejecuta

   -- Se crea det eliminando espacio en blanco que se genera cuando
   -- se pegan los 2 archivos

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "/;sed 's/	//g' det1 > det_vol_rc "
   RUN ejecuta

   -- Se suben los datos del detalle

   LET ejecuta = "cd ",vruta_ejecuta CLIPPED,
                 "/;DBDATE=y4md;export DBDATE;",
                 "dbload -d safre_af -c sube_voldet_rc -l /tmp/",
                 usuario CLIPPED,".VOL_DET_RC.log;"

   RUN ejecuta

   --------------------   GENERA DETALLE  08 --------------------

   LET vreporte = vruta_rescate CLIPPED,"/vfolio_det"

   START REPORT salida1 TO vreporte
      LET vtipo_registro = "08"

      OUTPUT TO REPORT salida1(v_folio_cod,
                               vtipo_registro
                              )

   FINISH REPORT salida1

   -- Se crea archivo adi que contiene a\  y se pega en vfolio_det

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; cat adi vfolio_det > vfolio_det1" CLIPPED
   RUN ejecuta

   -- Se crea archivo vfolio_det con el numero de registros igual al
   -- num. registros que tiene el detalle

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; sed -f vfolio_det1 det_vol_rc_cpp > vfolio_det2" CLIPPED
   RUN ejecuta

   -- Se crea archivo vfolio_det borrando lineas que no sirven

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; sed -e '/^ /!d' vfolio_det2 > vfolio_det3" CLIPPED
   RUN ejecuta

   -- Se crea det1 pegando datos genrales con el detalle

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; paste vfolio_det3 det_vol_rc_cpp > det1" CLIPPED
   RUN ejecuta

   -- Se crea det eliminando espacio en blanco que se genera cuando
   -- se pegan los 2 archivos

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "/;sed 's/	//g' det1 > det_vol_rc_cpp "
   RUN ejecuta

   -- Se suben los datos del detalle

   LET ejecuta = "cd ",vruta_ejecuta CLIPPED,
                 "/;DBDATE=y4md;export DBDATE;",
                 "dbload -d safre_af -c sube_voldet_rc_cpp -l /tmp/",
                 usuario CLIPPED,".VOL_DET_RC_CPP.log;"

   RUN ejecuta

   --------------------   GENERA SUMARIO   --------------------

   LET vreporte = vruta_rescate CLIPPED,"/vfolio_sum"

   START REPORT salida1 TO vreporte
      LET vtipo_registro = "09"

      OUTPUT TO REPORT salida1(v_folio_cod,
                              vtipo_registro
                              )
   FINISH REPORT salida1

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; paste vfolio_sum sum_vol.394 > sum1" CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                    "/;sed 's/	//g' sum1 > sum_vol.394 "
   RUN ejecuta

   LET ejecuta = "cd ",vruta_ejecuta CLIPPED,
                 "/;DBDATE=y4md;export DBDATE;",
                 "dbload -d safre_af -c sube_volrc_sum_394 -l /tmp/",
                 usuario CLIPPED,".VOLRC_SUM.log;"
   RUN ejecuta

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "/;DBDATE=mdy4;export DBDATE;"
   RUN ejecuta

   RETURN v_folio_cod
END FUNCTION


FUNCTION actualiza_registros(v_folio_cod)
#----------------------------------------------

   DEFINE deta RECORD LIKE int_det_vol_rc.*

   DEFINE #char
      lc_nss                   CHAR(11),
      lc_fecha_recep           CHAR(20),
      lc_letra                 CHAR(1)

   DEFINE #integer
      v_folio_cod              INTEGER

   DEFINE #smallint
      ls_cuantos               ,
      ls_result                SMALLINT

   DEFINE #datetime
      ld_fecha_recep           DATETIME YEAR TO SECOND

   DEFINE #decimal
      ld_tot_detalle           ,
      ld_tot_sum               ,
      ld_tot_cpp               DECIMAL(16,2)

   DEFINE ls_subcuenta SMALLINT --INV-4887

   INITIALIZE deta.* TO NULL
   LET ls_result = TRUE # Aceptado

   # Actualizando la fecha de carga en el encabezado
   UPDATE int_cza_volrc
   SET    fecha_carga = HOY
   WHERE  folio = v_folio_cod

   # Actualizando los montos de los importes, se divide entre 100
   UPDATE int_det_vol_rc
   SET    monto = monto / 100
   WHERE  folio = v_folio_cod

   UPDATE int_det_vol_rc_08
   SET    importe_subcta = importe_subcta / 100
   WHERE  folio          = v_folio_cod

   UPDATE int_sum_vol_rc
   SET    total_aport_vol = total_aport_vol / 100,
          total_benef_vol = total_benef_vol / 100
   WHERE  folio          = v_folio_cod


   DECLARE cur1 CURSOR FOR
   SELECT *
   FROM   int_det_vol_rc a
   WHERE  a.folio = v_folio_cod

   FOREACH cur1 INTO deta.*
      # Actualizando la fecha recepcion de cada detalle
      LET lc_fecha_recep = deta.fecha_recep_c[1,4], "-",
                           deta.fecha_recep_c[5,6], "-",
                           deta.fecha_recep_c[7,8], " ",
                           deta.fecha_recep_c[9,10], ":",
                           deta.fecha_recep_c[11,12], ":",
                           deta.fecha_recep_c[13,14]

      LET ld_fecha_recep = lc_fecha_recep

      UPDATE int_det_vol_rc
      SET    fecha_recep   = lc_fecha_recep
      WHERE  folio         = v_folio_cod
      AND    fecha_recep_c = deta.fecha_recep_c

      # Buscando la Cuenta Individual del trabajador
      LET ls_cuantos = 0

      DECLARE cur_mar CURSOR FOR
         SELECT n_seguro
         FROM   afi_mae_afiliado
         WHERE  n_unico  = deta.curp

      FOREACH cur_mar INTO lc_nss
         LET ls_cuantos = ls_cuantos + 1

         # Validando que no se encuentre inhabilitada por unificación
         SELECT unique "OK"
         FROM   cta_act_marca
         WHERE  nss = lc_nss
         AND    marca_cod = 130

         IF SQLCA.SQLCODE <> 0 THEN

            EXIT FOREACH
         END IF

      END FOREACH

      IF ls_cuantos = 0 THEN
         UPDATE int_det_vol_rc
         SET    estado    = 0   # Rechazado
         WHERE  curp      = deta.curp
         AND    folio     = v_folio_cod
      ELSE
         # Validando que los NSS del archivo y de SAFRE correspondan, esto solo para los NSS's y no para NTI's
         LET lc_letra = lc_nss[1,2]
         IF lc_letra = 'I' THEN
            UPDATE int_det_vol_rc
            SET    nss       = lc_nss
            WHERE  curp      = deta.curp
            AND    folio     = v_folio_cod
            AND    consecutivo = deta.consecutivo
         ELSE
            # Validando que el NSS reportado en el archivo sea igual al que maneja SAFRE
            IF lc_nss <> deta.nss THEN
               UPDATE int_det_vol_rc
               SET    estado    = 0   # Rechazado
               WHERE  curp      = deta.curp
               AND    folio     = v_folio_cod
               AND    consecutivo = deta.consecutivo
            END IF
         END IF
      END IF

      # Validando el Tipo de Aportacion
      IF deta.tipo_apor <> 1 AND    --INV-4887
      	 deta.tipo_apor <> 2 AND
      	 deta.tipo_apor <> 3 AND
      	 deta.tipo_apor <> 4 THEN   --INV-4887
         --ACTUALIZA NSS Y ESTADO AL SER ENCONTRADO EL NSS DE LA CURP
         UPDATE int_det_vol_rc
         SET    estado      = 0   # Rechazado
         WHERE  curp        = deta.curp
         AND    folio       = v_folio_cod
         AND    consecutivo = deta.consecutivo
      END IF

      ---- cambio APP Movil INV-4621
      ---- se actualiza nuevo tipo de movimiento 310 en vez de tipo de movimiento 012
      --IF deta.cve_rc = '012' THEN
      --   UPDATE int_det_vol_rc
      --   SET    subcuenta = 10 -- 'Aportación Voluntaria Redes Comerciales SWAP'
      --   WHERE  curp        = deta.curp
      --   AND    folio       = v_folio_cod
      --   AND    consecutivo = deta.consecutivo
      --END IF
      --
      ---- Actualizar la subcuenta para MILLAS PARA EL RETIRO --INV-4774
      --IF deta.cve_rc = '016' THEN
      --	 UPDATE int_det_vol_rc
      --   SET    subcuenta = 12 -- 'MILLAS PARA EL RETIRO'
      --   WHERE  curp        = deta.curp
      --   AND    folio       = v_folio_cod
      --   AND    consecutivo = deta.consecutivo
      --END IF

      #Actualizar subcuenta --INV-4887
      INITIALIZE ls_subcuenta TO NULL

      SELECT subcuenta
      INTO   ls_subcuenta
      FROM   tab_vol_redcom_subcta
      WHERE  tipo_apor = deta.tipo_apor
      AND    medio     = deta.medio     -- #CPL-3436
      AND    cve_red   = deta.cve_rc

      --DISPLAY "deta.tipo_apor: ", deta.tipo_apor
      --DISPLAY "deta.cve_rc   : ", deta.cve_rc
      --DISPLAY "ls_subcuenta  : ", ls_subcuenta

      IF ls_subcuenta IS NULL THEN
      	 #Si no está en el catálogo normal, buscar en las excepciones #INV-5323
      	 SELECT subcuenta
         INTO   ls_subcuenta
         FROM   tab_vol_redcom_subcta_excep
         WHERE  tipo_apor = deta.tipo_apor
         AND    medio     = deta.medio     -- #CPL-3436
         AND    cve_red  <> deta.cve_rc

         #Si no está en el catálogo normal, ni en las excepciones #INV-5323
         IF ls_subcuenta IS NULL THEN
      	    LET ls_subcuenta = 10
         END IF
      END IF

      UPDATE int_det_vol_rc
      SET    subcuenta = ls_subcuenta
      WHERE  curp        = deta.curp
      AND    folio       = v_folio_cod
      AND    consecutivo = deta.consecutivo
   END FOREACH

   # Validando que la suma de las aportaciones sea igual a la suma reportada en el detalle.
   SELECT SUM(monto)
   INTO   ld_tot_detalle
   FROM   int_det_vol_rc
   WHERE  folio =  v_folio_cod

   SELECT SUM(importe_subcta)
   INTO   ld_tot_cpp
   FROM   int_det_vol_rc_08
   WHERE  folio =  v_folio_cod

   SELECT total_aport_vol
   INTO   ld_tot_sum
   FROM   int_sum_vol_rc
   WHERE  folio =  v_folio_cod

   IF ld_tot_detalle <> ld_tot_sum OR
      ld_tot_detalle <> ld_tot_cpp THEN
      # En caso de no coincidir los montos se rechaza el archivo
      LET ls_result = FALSE
      CALL reversar_carga(v_folio_cod)
   END IF

   RETURN ls_result, ld_tot_sum, ld_tot_detalle, ld_tot_cpp
END FUNCTION


REPORT salida(vcontenido)
#------------------------

   DEFINE
      vcontenido    CHAR(1)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
         PRINT vcontenido,
               "\\"
END REPORT


REPORT salida1(v_folio_cod,
               vtipo_registro
              )
#---------------------------------

   DEFINE v_folio_cod          INTEGER,
          vtipo_registro       CHAR(02)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
             PRINT v_folio_cod    USING '----------'
                  #,vtipo_registro
END REPORT

FUNCTION consultar()
#-------------------
   DEFINE #date
      ld_fecha_carga    DATE

   DEFINE #integer
      li_folio          ,
      li_folio_cons       INTEGER

   DEFINE #smallint
      ls_total       ,
      ls_estado      ,
      ls_indx        SMALLINT

   DEFINE #char
      lc_query       ,
      ls_query_folio CHAR(500)

   DEFINE
      ld_monto_tot   DECIMAL(16,2)

   DEFINE lr_result ARRAY[2] OF RECORD
      seleccion      CHAR(1)       ,
      ls_num_reg     SMALLINT      ,
      ld_sum_monto   DECIMAL(16,2)
   END RECORD

   OPEN WINDOW v3 AT 5,3 WITH FORM "VOLB0142" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " VOLB014         APORTACIONES VOLUNTARIAS CON REDES COMERCIALES             " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY   USING"DD-MM-YYYY " AT 2,66 ATTRIBUTE(REVERSE)

   LET ld_fecha_carga = HOY
   LET li_folio = NULL
   LET li_folio_cons = NULL
   INITIALIZE lr_result[1].* TO NULL
   INITIALIZE lr_result[2].* TO NULL

   INPUT BY NAME ld_fecha_carga, li_folio WITHOUT DEFAULTS
      AFTER FIELD li_folio
         IF ld_fecha_carga IS NULL AND
            li_folio IS NULL THEN
               ERROR " DEBE INCLUIR AL MENOS UN CRITERIO DE BÚSQUEDA "
               NEXT FIELD ld_fecha_carga
         END IF

     ON KEY (ESC)
         IF ld_fecha_carga IS NULL AND
               li_folio IS NULL THEN
                  ERROR " DEBE INCLUIR AL MENOS UN CRITERIO DE BÚSQUEDA "
                  NEXT FIELD ld_fecha_carga
         END IF

         # Validando que exista información con los parámetros proporcionados
         IF ld_fecha_carga IS NOT NULL AND
            li_folio IS NULL THEN
            SELECT unique folio
            INTO   li_folio_cons
            FROM   int_cza_volrc
            WHERE  fecha_carga = ld_fecha_carga

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR " NO SE ENCONTRÓ INFORMACIÓN DEL FOLIO ...."
               SLEEP 2
               NEXT FIELD ld_fecha_carga
            END IF
         END IF

         IF li_folio IS NOT NULL AND
            ld_fecha_carga IS NULL THEN
            SELECT unique folio
            INTO   li_folio_cons
            FROM   int_cza_volrc
            WHERE  folio = li_folio

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR " NO SE ENCONTRÓ INFORMACIÓN DEL FOLIO ...."
               SLEEP 2
               NEXT FIELD ld_fecha_carga
            END IF
         END IF

         IF li_folio IS NOT NULL AND
            ld_fecha_carga IS NOT NULL THEN
            SELECT unique folio
            INTO   li_folio_cons
            FROM   int_cza_volrc
            WHERE  folio = li_folio
            AND    fecha_carga = ld_fecha_carga

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR " NO SE ENCONTRÓ INFORMACIÓN DEL FOLIO ...."
               SLEEP 2
               NEXT FIELD ld_fecha_carga
            END IF
         END IF

         LET lc_query = " SELECT NVL(COUNT(nss),0), NVL(SUM(monto), 0)  \n",
                        " FROM   int_det_vol_rc                               \n",
                        " WHERE  estado = ?                                   \n",
                        " AND    folio = ", li_folio_cons,"                        \n",
                        " GROUP BY folio                                      \n"

         # Obteniendo el número de registros aceptados
         LET ls_estado = 1
         PREPARE p_acept FROM  lc_query
         EXECUTE p_acept USING ls_estado
                         INTO  lr_result[1].ls_num_reg, lr_result[1].ld_sum_monto

         # Obteniendo el número de registros rechazados
         LET ls_estado = 0
         PREPARE p_rech FROM  lc_query
         EXECUTE p_rech USING ls_estado
                        INTO  lr_result[2].ls_num_reg, lr_result[2].ld_sum_monto

         IF lr_result[1].ls_num_reg IS NULL AND
            lr_result[2].ls_num_reg IS NULL THEN

               ERROR " NO SE ENCONTRÓ INFORMACIÓN DEL FOLIO ...."
               SLEEP 2
               NEXT FIELD ld_fecha_carga
         END IF

         # Calculando el total de registros recibidos
         LET ls_total     = lr_result[1].ls_num_reg   + lr_result[2].ls_num_reg
         LET ld_monto_tot = lr_result[1].ld_sum_monto + lr_result[2].ld_sum_monto

         DISPLAY " <ESC> Ejecutar             <Ctrl-W> Detalle                <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
         DISPLAY BY NAME ls_total
         DISPLAY BY NAME ld_monto_tot

         CALL SET_COUNT( 2 )
         INPUT ARRAY lr_result WITHOUT DEFAULTS FROM scr_cons.*
            AFTER FIELD seleccion
               LET ls_indx = ARR_CURR()
            ON KEY( CONTROL-W )
               LET ls_indx = ARR_CURR()
               CALL mostrar_detalle_consulta(ls_indx, li_folio_cons)
            ON KEY (INTERRUPT, CONTROL-C)
               PROMPT " CONSULTA FINALIZADA...<ENTER> PARA SALIR " FOR enter
               CLEAR FORM
               EXIT INPUT
         END INPUT

      ON KEY (INTERRUPT, CONTROL-C)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR enter
         EXIT INPUT

   END INPUT
   CLOSE WINDOW v3
END FUNCTION

FUNCTION mostrar_detalle_consulta(ps_tipo_detalle, pi_folio)
#-------------------
   DEFINE # smallint
      ps_tipo_detalle      SMALLINT

   DEFINE #smallint
      ls_estado            ,
      ls_pos               SMALLINT

   DEFINE #integer
      pi_folio             INTEGER

   DEFINE #datatime
      ld_fecha_recep       LIKE int_det_vol_rc.fecha_recep

   DEFINE #decimal
      ld_monto_tot         DECIMAL(16,2)

   DEFINE lr_det ARRAY[500] OF RECORD
      curp                 LIKE int_det_vol_rc.curp        ,
      nss                  LIKE int_det_vol_rc.nss         ,
      fecha_recep_c        CHAR(10)                        ,
      monto                LIKE int_det_vol_rc.monto
   END RECORD

   OPEN WINDOW v4 AT 5,6 WITH FORM "VOLB0143" ATTRIBUTE(BORDER)
   DISPLAY " DETALLE                  <Ctrl-P> REPORTE           <Ctrl-C > Salir " AT 2,1 ATTRIBUTE(REVERSE)

   # Realizando la busqueda del detalle de las aportaciones
   CASE ps_tipo_detalle
      WHEN 1 # Aceptados
         LET ls_estado = 1
      WHEN 2 # Rechazados
         LET ls_estado = 0
   END CASE

   DECLARE cur_det CURSOR FOR
      SELECT curp,nss,
             fecha_recep,monto
      FROM   int_det_vol_rc
      WHERE  estado = ls_estado
      AND    folio  = pi_folio

   LET ls_pos = 1
   LET ld_monto_tot = 0
   FOREACH cur_det INTO lr_det[ls_pos].curp, lr_det[ls_pos].nss,
                        ld_fecha_recep, lr_det[ls_pos].monto

      LET lr_det[ls_pos].fecha_recep_c = DATE(ld_fecha_recep)
      LET ld_monto_tot = ld_monto_tot + lr_det[ls_pos].monto

      LET ls_pos = ls_pos + 1
   END FOREACH

   LET ls_pos = ls_pos - 1
   DISPLAY ls_pos   TO ls_total
   DISPLAY pi_folio TO folio
   DISPLAY BY NAME ld_monto_tot

   CALL SET_COUNT( ls_pos )
   DISPLAY ARRAY lr_det TO scr_det.*
      ON KEY( CONTROL-P )
               CALL genera_reporte_detalle(ls_estado, pi_folio)
               EXIT DISPLAY
      ON KEY (INTERRUPT, CONTROL-C)
         EXIT DISPLAY
   END DISPLAY

   CLOSE WINDOW v4
END FUNCTION

FUNCTION genera_reporte_detalle(ps_estado, pi_folio)
#-------------------
   DEFINE #integer
      pi_folio             INTEGER

   DEFINE #smallint
      ps_estado            SMALLINT

   DEFINE #char
      lc_nom_reporte       ,
      lc_mensaje           CHAR(1000),
      lc_tipo_rep          CHAR(1)

   DEFINE lr_detalle RECORD LIKE int_det_vol_rc.*

   CASE ps_estado
      WHEN 1
         LET lc_tipo_rep = "A"
      WHEN 0
         LET lc_tipo_rep = "R"
   END CASE

   LET lc_nom_reporte = vruta_listados CLIPPED,"/",
                 usuario CLIPPED,
                 ".", lc_tipo_rep, "VolRed",
                 HOY USING "ddmmyyyy"

   START REPORT rpt_detvol_rc TO lc_nom_reporte

   # Obteniendo los detalles del folio
   DECLARE cur_det_rep CURSOR FOR
      SELECT *
      FROM   int_det_vol_rc
      WHERE  folio  = pi_folio
      AND    estado = ps_estado

   FOREACH cur_det_rep INTO lr_detalle.*
      OUTPUT TO REPORT rpt_detvol_rc(ps_estado,lr_detalle.*)
   END FOREACH

   FINISH REPORT rpt_detvol_rc

   DISPLAY " REPORTE GENERADO: " AT 15,2
   DISPLAY lc_nom_reporte CLIPPED AT 16,5
   PROMPT " ...<ENTER> PARA SALIR " FOR enter
END FUNCTION

REPORT rpt_detvol_rc( ps_estado,pr_detalle)
#---------------------------------

   DEFINE pr_detalle RECORD LIKE int_det_vol_rc.*

   DEFINE ps_estado  SMALLINT

   DEFINE lc_fecha   CHAR(20),
          lc_tipo    CHAR(35)



   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  30

   FORMAT
      PAGE HEADER
         CASE ps_estado
            WHEN 1
               LET lc_tipo = "ACEPTADOS"
            WHEN 0
               LET lc_tipo = "RECHAZADOS - CUENTAS NO LOCALIZADAS"
         END CASE

         PRINT COLUMN 01," APORTACIONES VOLUNTARIAS POR REDES COMERCIALES "
         PRINT COLUMN 01," REPORTE (REGISTROS ", lc_tipo CLIPPED, ")"

         SKIP 1 LINE

         PRINT COLUMN 01,"FOLIO : ", pr_detalle.folio

         SKIP 1 LINE

         PRINT COLUMN 01,"-------------------------------------------------------------------------------------------------------------------------------------------------------------",
                         "-------------------------------------------------------------------------------------------------------------------------------------------------------------"

         PRINT COLUMN 007,"CURP",
               COLUMN 026,"NSS",
               COLUMN 073,"NOMBRE",
               COLUMN 131,"NUM CELULAR",
               COLUMN 146,"COMPAÑIA",
               COLUMN 163,"FECHA RECEP",
               COLUMN 178,"FECHA PAGO",
               COLUMN 192,"FECHA VALOR",
               COLUMN 207,"FOLIO AUT",
               COLUMN 220,"CVE RED COM",
               COLUMN 235,"SUC RED COM",
               COLUMN 263,"MONTO",
               COLUMN 273,"TIPO APORTACION",
               COLUMN 295,"CURP ANT",
               --COLUMN 312,"TIPO DE AHORRADOR" -- #CPL-3436
               COLUMN 312,"MEDIO CAPTACION"     -- #CPL-3436

         PRINT COLUMN 01,"-------------------------------------------------------------------------------------------------------------------------------------------------------------",
                         "-------------------------------------------------------------------------------------------------------------------------------------------------------------"

         SKIP 1 LINE

      ON EVERY ROW

         PRINT COLUMN 001,pr_detalle.curp                                      ,
               COLUMN 022,pr_detalle.nss                                       ,
               COLUMN 037,pr_detalle.paterno        CLIPPED, " "               ,
                          pr_detalle.materno        CLIPPED, " "               ,
                          pr_detalle.nombre         CLIPPED                    ,
               COLUMN 132,pr_detalle.celular                                   ,
               COLUMN 150,pr_detalle.compania_tel   USING "<<<<<"              ,
               COLUMN 156,pr_detalle.fecha_recep                               ,
               COLUMN 179,pr_detalle.fecha_pago                                ,
               COLUMN 193,pr_detalle.fecha_valor                               ,
               COLUMN 205,pr_detalle.folio_aut                                 ,
               COLUMN 225,pr_detalle.cve_rc                                    ,
               COLUMN 238,pr_detalle.sucursal                                  ,
               COLUMN 255,pr_detalle.monto                                     ,
               COLUMN 279,pr_detalle.tipo_apor      USING "<<<<<"              ,
               COLUMN 292,pr_detalle.curp_ant                                  ,
               --COLUMN 318,pr_detalle.tipo_de_ahorrador -- #CPL-3436
               COLUMN 318,pr_detalle.medio               -- #CPL-3436


      ON LAST ROW

         PRINT COLUMN 01,"-------------------------------------------------------------------------------------------------------------------------------------------------------------",
                         "-------------------------------------------------------------------------------------------------------------------------------------------------------------"

         SKIP 3 LINES

         PRINT COLUMN 01," TOTAL DE REGISTROS : ",COUNT(*) USING "<<<<<<<"

END REPORT

FUNCTION liquida()
#-------------------
   DEFINE #integer
      li_folio          INTEGER

   DEFINE #date
      fecha_liq         DATE

   DEFINE #smallint
      ls_siefore        ,
      ls_indx           ,
      ls_result         SMALLINT

   DEFINE #decimal
      ld_monto_acc      ,
      ld_precio_acc     DECIMAL(16,6),
      ld_monto_pesos    DECIMAL(16,2)

   DEFINE ls_cuantos,ls_flag SMALLINT

   OPEN WINDOW v5 AT 5,3 WITH FORM "VOLB0144" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " VOLB014         APORTACIONES VOLUNTARIAS CON REDES COMERCIALES             " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY   USING"DD-MM-YYYY " AT 2,66 ATTRIBUTE(REVERSE)

   LET fecha_liq = HOY
   LET li_folio     = NULL
   LET INT_FLAG  = FALSE

   INPUT BY NAME li_folio, fecha_liq WITHOUT DEFAULTS
      AFTER FIELD li_folio
         IF li_folio IS NULL THEN
               ERROR " FOLIO NO PUEDE SER NULO "
               NEXT FIELD li_folio
         END IF

         #Buscando la información del folio
         SELECT unique "OK"
         FROM   int_cza_volrc
         WHERE  folio = li_folio

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL FOLIO...  "
            LET  li_folio = NULL
            NEXT FIELD li_folio
         END IF

      AFTER FIELD fecha_liq
         IF fecha_liq IS NULL THEN
               ERROR " FECHA LIQUIDACIÓN NO PUEDE SER NULA "
               NEXT FIELD fecha_liq
         END IF

         SELECT unique "OK"
         FROM   int_det_vol_rc a, cta_regimen b, glo_valor_accion c
         WHERE  a.nss             = b.nss
         AND    a.folio           = li_folio
         AND    a.subcuenta       = b.subcuenta
         AND    a.estado          = 1
         AND    c.fecha_valuacion = fecha_liq
         AND    c.codigo_siefore  = b.codigo_siefore

         IF STATUS = NOTFOUND THEN
            ERROR " NO EXISTEN PRECIOS DE ACCION PARA LA FECHA SELECCIONADA "
            NEXT FIELD fecha_liq
         END IF

         ERROR ""

      ON KEY (ESC)
         IF li_folio IS NULL THEN
               ERROR " FOLIO NO PUEDE SER NULO "
               NEXT FIELD li_folio
         END IF

         #Buscando la información del folio
         SELECT unique "OK"
         FROM   int_cza_volrc
         WHERE  folio = li_folio

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL FOLIO...  "
               NEXT FIELD li_folio
         END IF

         #Buscando la información del folio
         SELECT unique "OK"
         FROM   dis_cuenta
         WHERE  folio = li_folio

         IF SQLCA.SQLCODE = 0 THEN
            ERROR " FOLIO LIQUIDADO ANTERIORMENTE...  "
               NEXT FIELD li_folio
         END IF

         IF fecha_liq IS NULL THEN
               ERROR " FECHA LIQUIDACIÓN NO PUEDE SER NULA "
               NEXT FIELD fecha_liq
         END IF

         SELECT unique "OK"
         FROM   int_det_vol_rc a, cta_regimen b, glo_valor_accion c
         WHERE  a.nss             = b.nss
         AND    a.folio           = li_folio
         AND    a.subcuenta       = b.subcuenta
         AND    a.estado          = 1
         AND    c.fecha_valuacion = fecha_liq
         AND    c.codigo_siefore  = b.codigo_siefore

         IF STATUS = NOTFOUND THEN
            ERROR " NO EXISTEN PRECIOS DE ACCION PARA LA FECHA SELECCIONADA "
            NEXT FIELD fecha_liq
         END IF


         EXIT INPUT
      ON KEY( INTERRUPT, CONTROL-C )
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR enter
   ELSE
   	  --INV-4621
   	  --Mostrando los totales a preliquidar
   	  LET ls_cuantos = fn_cifras_previas(li_folio,fecha_liq)

   	  --Si no hay registros
   	  IF ls_cuantos = 0 THEN
         PROMPT "NO HAY INFORMACIÓN POR PRELIQUIDAR... <ENTER>PARA SALIR "
         FOR enter
         CLOSE WINDOW v5
         RETURN
   	  END IF

      DISPLAY "<ESC> Liquidar              MONTOS A LIQUIDAR             <Ctrl-C > Cancelar" AT 10,1 ATTRIBUTE(REVERSE)
      DISPLAY "SUBCTA SIEFORE             ACCIONES             PRECIO                PESOS " AT 11,1 ATTRIBUTE(REVERSE)

      CALL SET_COUNT(ls_cuantos)
      DISPLAY ARRAY gar_cifras_prev TO scr_cifras.*
         ON KEY(CONTROL-C, INTERRUPT)
         	 LET ls_flag = 1
         	 EXIT DISPLAY

         ON KEY(ESC)
            LET ls_flag = 0
         	  EXIT DISPLAY
      END DISPLAY

      IF ls_flag <> 0 THEN
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR enter
         CLOSE WINDOW v5
         RETURN
      END IF


      WHILE TRUE
         PROMPT " DESEA EJECUTAR LA LIQUIDACIÓN [S/N] ? : " FOR enter
         IF enter MATCHES "[Ss]" THEN
            CALL ejecuta_liquidacion(li_folio, fecha_liq)
               RETURNING ls_result
            IF ls_result = 0 THEN
               CALL fn_actualiza_edos_liq(li_folio) #CPL-3641
            END IF
            EXIT WHILE
         ELSE
            IF enter MATCHES "[Nn]" THEN
               LET INT_FLAG = TRUE
               EXIT WHILE
            END IF
         END IF
      END WHILE

      IF ls_result = 0 THEN
          PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR "
          FOR enter
      END IF
   END IF
   CLOSE WINDOW v5
END FUNCTION

FUNCTION reverso()
#-------------------
   DEFINE #smallint
      opc_reverso       SMALLINT

   DEFINE #integer
      li_folio          INTEGER


   OPEN WINDOW v6 AT 5,3 WITH FORM "VOLB0145" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " VOLB014         APORTACIONES VOLUNTARIAS CON REDES COMERCIALES             " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY   USING"DD-MM-YYYY " AT 2,66 ATTRIBUTE(REVERSE)

   INITIALIZE opc_reverso TO NULL
   INITIALIZE li_folio TO NULL

   INPUT BY NAME opc_reverso,li_folio WITHOUT DEFAULTS
      AFTER FIELD opc_reverso
         IF opc_reverso IS NULL THEN
            ERROR " OPC REVERSO NO PUEDE SER NULO "
            NEXT FIELD opc_reverso
         END IF

      AFTER FIELD li_folio
         IF opc_reverso IS NULL THEN
            ERROR " OPC REVERSO NO PUEDE SER NULO "
            NEXT FIELD opc_reverso
         END IF

         IF li_folio IS NULL THEN
            ERROR " FOLIO NO PUEDE SER NULO "
            NEXT FIELD li_folio
         END IF
      ON KEY (ESC)
         IF opc_reverso IS NULL THEN
            ERROR " OPC REVERSO NO PUEDE SER NULO "
            NEXT FIELD opc_reverso
         END IF

         IF li_folio IS NULL THEN
            ERROR " FOLIO NO PUEDE SER NULO "
            NEXT FIELD li_folio
         END IF

         CASE opc_reverso
            WHEN 1
               # Validando que exista el folio
               SELECT unique "OK"
               FROM   int_cza_volrc
               WHERE  folio = li_folio

               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR " NO EXISTE INFORMACION DE LA CARGA DEL FOLIO...  "
                  NEXT FIELD li_folio
               END IF

               # Validando que el folio no se encuentre liquidado
               SELECT unique "OK"
               FROM   dis_cuenta
               WHERE  folio = li_folio

               IF SQLCA.SQLCODE = 0 THEN
                  ERROR " LA CARGA NO PUEDE SER REVERSADA, FOLIO LIQUIDADO  "
                  NEXT FIELD li_folio
               END IF

               WHILE TRUE
                  PROMPT " DESEA EJECUTAR EL REVERSO [S/N] ? : " FOR enter
                  IF enter MATCHES "[Ss]" THEN
                     CALL reversar_carga(li_folio)
                     EXIT WHILE
                  ELSE
                     IF enter MATCHES "[Nn]" THEN
                        EXIT WHILE
                     END IF
                  END IF
               END WHILE

            WHEN 2
               # Validando que el folio se encuentre liquidado
               SELECT unique "OK"
               FROM   dis_cuenta
               WHERE  folio = li_folio

               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR " NO EXISTE INFORMACION DE LIQUIDACIÓN...  "
                  NEXT FIELD li_folio
               END IF

               WHILE TRUE
                  PROMPT " DESEA EJECUTAR EL REVERSO [S/N] ? : " FOR enter
                  IF enter MATCHES "[Ss]" THEN
                     CALL reversar_liquidacion(li_folio)
                     CALL fn_reversa_edos_liq(li_folio) #CPL-3641
                     EXIT WHILE
                  ELSE
                     IF enter MATCHES "[Nn]" THEN
                        EXIT WHILE
                     END IF
                  END IF
               END WHILE
         END CASE
         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR enter
         EXIT INPUT
      ON KEY (INTERRUPT, CONTROL-C)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR enter
         EXIT INPUT
   END INPUT

   CLOSE WINDOW v6
END FUNCTION

FUNCTION reversar_carga(pi_folio)
#-------------------
   DEFINE #integer
      pi_folio          INTEGER

   ERROR "REVERSANDO CARGA..."
   # Eliminando la información de las tablas de Encabezado, Detalles y Sumario
   # Encabezado
   DELETE
   FROM   int_cza_volrc
   WHERE  folio = pi_folio

   # Detalle
   DELETE
   FROM   int_det_vol_rc
   WHERE  folio = pi_folio

   # Detalle cuentas por pagar
   DELETE
   FROM   int_det_vol_rc_08
   WHERE  folio = pi_folio

   # Sumario
   DELETE
   FROM   int_sum_vol_rc
   WHERE  folio = pi_folio

   # Eliminando el registro de los arhivos de voluntarias
   DELETE
   FROM   int_archivo_vol
   WHERE  folio = pi_folio
   ERROR ""
END FUNCTION

FUNCTION reversar_liquidacion(pi_folio)
#-------------------
   DEFINE #integer
      pi_folio          INTEGER

   ERROR "REVERSANDO LIQUIDACION..."
   # Eliminando la información liquidada
   DELETE
   FROM   dis_cuenta
   WHERE  folio = pi_folio

   ERROR ""
END FUNCTION

FUNCTION ejecuta_liquidacion(pi_folio, pd_fecha_liq)
#-----------------------------

   DEFINE #integer
      pi_folio             INTEGER

   DEFINE #smallint
      ls_tot_reg           ,
      ls_result            SMALLINT

   DEFINE #date
      pd_fecha_liq         DATE

   DEFINE lr_liquida RECORD
      nss                  LIKE  int_det_vol_rc.nss            ,
      siefore              LIKE  cta_regimen.codigo_siefore    ,
      monto_en_pesos       LIKE  dis_cuenta.monto_en_pesos     ,
      monto_en_acciones    LIKE  dis_cuenta.monto_en_acciones  ,
      precio_accion        LIKE  dis_cuenta.precio_accion      ,
      sucursal             LIKE  int_det_vol_rc.sucursal       , --INV-4621
      consecutivo          LIKE  int_det_vol_rc.consecutivo    , --INV-4621
      tipo_apor            INTEGER                             , --INV-4887
      tipo_de_ahorrador    CHAR(02)                            , --INV-4621
      curp                 CHAR(18)                            , --INV-4621
      subcuenta            SMALLINT                            , --INV-4621
      tipo_mov             SMALLINT                            , --INV-4621
      id_aportante         CHAR(11)                            ,
      cve_rc               CHAR(03)                            ,
      medio                SMALLINT                              -- #CPL-3436
   END RECORD

   DEFINE ls_tipo_mov     SMALLINT
   DEFINE lc_id_aportante CHAR(11)

   DECLARE cur_liq CURSOR FOR
   SELECT a.nss                                       ,
          a.curp                                         ,
          a.subcuenta                                    ,
          b.codigo_siefore                               ,
          a.monto                                        ,
          a.sucursal                                     ,
          a.consecutivo                                  ,
          a.tipo_apor                                    ,
          NVL(a.tipo_de_ahorrador,'00') tipo_de_ahorrador,
          123                           tipo_mov         ,
          'VE-REDCO'                    id_aportante     ,
          a.cve_rc                                       ,
          a.medio                                        -- #CPL-3436
   FROM   int_det_vol_rc a,
          cta_regimen    b
   WHERE  a.nss       = b.nss
   AND    a.folio     = pi_folio
   AND    a.subcuenta = b.subcuenta
   AND    a.estado    = 1
   ORDER BY 1

   LET ls_tot_reg = 0

   FOREACH cur_liq INTO lr_liquida.nss              , --nss
   	                    lr_liquida.curp             , --curp
   	                    lr_liquida.subcuenta        , --subcuenta
   	                    lr_liquida.siefore          , --codigo_siefore
                        lr_liquida.monto_en_pesos   , --monto
                        lr_liquida.sucursal         , --sucursal
                        lr_liquida.consecutivo      , --consecutivo
                        lr_liquida.tipo_apor        , --tipo_apor
                        lr_liquida.tipo_de_ahorrador, --L(a.tipo_de_ahotipo_de_ahorrador
                        lr_liquida.tipo_mov         , --tipo_mov
                        lr_liquida.id_aportante     , --id_aportante
                        lr_liquida.cve_rc           , --cve_rc
                        lr_liquida.medio              --medio #CPL-3436

      ---- INV-4621
      ---- Para los trabajadores SWAP
      ---- Cambiar id_aportante y tipo_mov
      --IF lr_liquida.cve_rc = '012' THEN
      --   LET lr_liquida.tipo_mov     = 310
      --   LET lr_liquida.id_aportante = 'VE-REDAPP'
      --END IF

      #Actualizar id_aportante y mov --INV-4887
      INITIALIZE ls_tipo_mov, lc_id_aportante TO NULL

      SELECT tipo_mov,
             id_aportante
      INTO   ls_tipo_mov,
             lc_id_aportante
      FROM   tab_vol_redcom_subcta
      WHERE  tipo_apor = lr_liquida.tipo_apor
      AND    medio     = lr_liquida.medio     -- #CPL-3436
      AND    cve_red   = lr_liquida.cve_rc

      IF ls_tipo_mov IS NULL THEN
         #Si no está en el catálogo normal, buscar en las excepciones #INV-5323
         SELECT tipo_mov,
                id_aportante
         INTO   ls_tipo_mov,
                lc_id_aportante
         FROM   tab_vol_redcom_subcta_excep
         WHERE  tipo_apor = lr_liquida.tipo_apor
         AND    medio     = lr_liquida.medio     -- #CPL-3436
         AND    cve_red  <> lr_liquida.cve_rc

         IF ls_tipo_mov IS NULL THEN
         #Si no está en el catálogo normal, ni en las excepciones #INV-5323
            LET ls_tipo_mov     = 123
            LET lc_id_aportante = 'VE-REDCO'
         END IF
      END IF

      LET lr_liquida.tipo_mov     = ls_tipo_mov
      LET lr_liquida.id_aportante = lc_id_aportante

      # Obteniendo el precio de accion para la siefore
      SELECT precio_del_dia
      INTO   lr_liquida.precio_accion
      FROM   glo_valor_accion
      WHERE  codigo_siefore  = lr_liquida.siefore
      AND    fecha_valuacion = pd_fecha_liq

      LET lr_liquida.monto_en_acciones = lr_liquida.monto_en_pesos / lr_liquida.precio_accion

      LET ls_tot_reg = ls_tot_reg + 1

      # Insertando en dis_cuenta
      CALL agrega_liquidacion(pi_folio,
                              pd_fecha_liq,
                              lr_liquida.*)
   END FOREACH

   LET ls_result = 0

   IF ls_tot_reg = 0 THEN
      ERROR ""
      PROMPT " NO EXISTE INFORMACION...<ENTER> PARA SALIR " FOR CHAR enter
      LET ls_result = 1
      RETURN ls_result
   END IF

   ERROR ""
   RETURN ls_result

END FUNCTION

FUNCTION agrega_liquidacion(pi_folio, pd_fecha_liq,pr_liquida)
#-------------------------------------------------------------------

  DEFINE pr_liquida RECORD
      nss                  LIKE  int_det_vol_rc.nss            ,
      siefore              LIKE  cta_regimen.codigo_siefore    ,
      monto_en_pesos       LIKE  dis_cuenta.monto_en_pesos     ,
      monto_en_acciones    LIKE  dis_cuenta.monto_en_acciones  ,
      precio_accion        LIKE  dis_cuenta.precio_accion      ,
      sucursal             LIKE  int_det_vol_rc.sucursal       , --INV-4621
      consecutivo          LIKE  int_det_vol_rc.consecutivo    , --INV-4621
      tipo_apor            INTEGER                             , --INV-4887
      tipo_de_ahorrador    CHAR(02)                            , --INV-4621
      curp                 CHAR(18)                            , --INV-4621
      subcuenta            SMALLINT                            , --INV-4621
      tipo_mov             SMALLINT                            , --INV-4621
      id_aportante         CHAR(11)                            ,
      cve_rc               CHAR(03)                            ,
      medio                SMALLINT                              -- #CPL-3436
   END RECORD

   DEFINE #date
      pd_fecha_liq         DATE

   DEFINE lc_tipo_reg      CHAR(03)

   DEFINE
      pi_folio             INTEGER

     INSERT INTO dis_cuenta
     VALUES( pr_liquida.tipo_mov         ,  --tipo_movimiento
             pr_liquida.subcuenta        ,  --subcuenta
             pr_liquida.siefore          ,  --siefore
             pi_folio                    ,  --folio
             pr_liquida.consecutivo      ,  --consecutivo_lote
             pr_liquida.nss              ,  --nss
             pr_liquida.curp             ,  --curp
             " "                         ,  --folio_sua
             pd_fecha_liq                ,  --fecha_pago
             pd_fecha_liq                ,  --fecha_valor
             pd_fecha_liq                ,  --fecha_conversion
             pr_liquida.monto_en_pesos   ,  --monto_en_pesos
             pr_liquida.monto_en_acciones,  --monto_en_acciones
             pr_liquida.precio_accion    ,  --precio_accion
             0                           ,  --dias_cotizados
             pr_liquida.sucursal         ,  --sucursal
             pr_liquida.id_aportante     ,  --id_aportante
             0                           ,  --estado
             HOY                         ,  --fecha_proceso
             USER                        ,  --usuario
             HOY                         ,  --fecha_archivo
             0 )                            --etiqueta

END FUNCTION
################################################################################
FUNCTION fn_cifras_previas(li_folio, ld_fecha_liq)
   DEFINE li_folio     INTEGER
   DEFINE ld_fecha_liq DATE
   DEFINE ls_cuantos   SMALLINT

   FOR ls_cuantos = 1 TO 500 #INV-5470
      INITIALIZE gar_cifras_prev[ls_cuantos].* TO NULL
   END FOR

   DECLARE cur_cifras_prev CURSOR FOR
   SELECT a.subcuenta     ,
          b.codigo_siefore,
          SUM(a.monto)
   FROM   int_det_vol_rc a,
          cta_regimen     b
   WHERE  a.nss       = b.nss
   AND    a.folio     = li_folio
   AND    a.subcuenta = b.subcuenta
   AND    a.estado    = 1
   GROUP BY 1,2
   ORDER BY 1,2

   LET ls_cuantos = 1
   FOREACH cur_cifras_prev INTO gar_cifras_prev[ls_cuantos].subcuenta,
   	                            gar_cifras_prev[ls_cuantos].siefore  ,
   	                            gar_cifras_prev[ls_cuantos].pesos

      # Obteniendo el precio de accion para la siefore
      SELECT NVL(SUM(precio_del_dia),0)
      INTO   gar_cifras_prev[ls_cuantos].precio
      FROM   glo_valor_accion
      WHERE  codigo_siefore  = gar_cifras_prev[ls_cuantos].siefore
      AND    fecha_valuacion = ld_fecha_liq

      # Calculando el monto en acciones
      LET gar_cifras_prev[ls_cuantos].acciones = gar_cifras_prev[ls_cuantos].pesos /
                                                 gar_cifras_prev[ls_cuantos].precio

      LET ls_cuantos = ls_cuantos + 1
   END FOREACH

   LET ls_cuantos = ls_cuantos - 1

   RETURN ls_cuantos
END FUNCTION
################################################################################
FUNCTION fn_actualiza_edos_liq(li_folio)
   DEFINE li_folio INTEGER

   UPDATE int_det_vol_rc
   SET    estado    = 2   #Liquidado
   WHERE  folio     = li_folio
   AND    estado    = 1   #Aceptado
END FUNCTION
################################################################################
FUNCTION fn_reversa_edos_liq(li_folio)
   DEFINE li_folio INTEGER

   UPDATE int_det_vol_rc
   SET    estado    = 1   #Aceptado
   WHERE  folio     = li_folio
   AND    estado    = 2   #Liquidado
END FUNCTION
################################################################################