###############################################################################
#Proyecto     => safre_af
#Propietario  => E.F.P.
#Programa     => VOLB015
#Descripcion  => LANZADOR DE CONSULTA DE APORTACIONES DE AHORRO VOLUNTARIOS CON BENEFICIO FISCAL
#Fecha        => 2023
#Por          => CÉSAR DAVID CHÁVEZ MARTÍNEZ
#Sistema      => VOL
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE gr_seg_modulo RECORD
   	   modulo_cod    CHAR(04),
       ruta_rescate  CHAR(40),
       ruta_listados CHAR(40)
   END RECORD

   DEFINE gc_usuario CHAR(08)
   DEFINE gs_afore   SMALLINT

   DEFINE gr_parametros RECORD
  	  cve_proceso    CHAR(07),
  	  folio          INTEGER ,
  	  archivo_salida CHAR(50),
  	  archivo_error  CHAR(50),
  	  programa       CHAR(20)
  END RECORD

  DEFINE gr_captura RECORD
     fecha_ini DATE    ,
     fecha_fin DATE
  END RECORD

  DEFINE gr_pasos RECORD
     carga        ,
     diagnostico  ,
     provision    ,
     regimen      ,
     liquidacion  SMALLINT
  END RECORD

  DEFINE HOY DATE

  DEFINE gar_precios ARRAY[100] OF RECORD
      precio  DECIMAL(19,14)
  END RECORD

  DEFINE gs_siefore_rec SMALLINT

  DEFINE gr_mov RECORD
      cargo,
      abono SMALLINT
   END RECORD

   DEFINE gar_cifras  ARRAY[11000] OF RECORD
      siefore    SMALLINT     ,
      subcuenta  SMALLINT     ,
      movimiento SMALLINT     ,
      pesos      DECIMAL(22,6),
      acciones   DECIMAL(22,6)
   END RECORD
END GLOBALS


################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".VOLB015.log")

   CALL init()
   CALL consulta()
END MAIN
################################################################################
FUNCTION init()
   LET gr_seg_modulo.modulo_cod = "vol"

   SELECT USER         ,
          ruta_rescate ,
          ruta_listados
   INTO   gc_usuario                 ,
          gr_seg_modulo.ruta_rescate ,
          gr_seg_modulo.ruta_listados
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   SELECT codigo_afore
   INTO   gs_afore
   FROM   safre_af:tab_afore_local

   LET hoy    = TODAY
   LET gr_parametros.cve_proceso = 'VOLB015'

END FUNCTION
################################################################################
FUNCTION consulta()
   DEFINE ls_proceso SMALLINT

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "VOLB0151" ATTRIBUTE(BORDER)
   DISPLAY "   REPORTE DE LIQUIDACION DE APORTACIONES VOLUNTARIAS CON BENEFICIO FISCAL    " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY "VOLB015                                                                " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)
   DISPLAY "<ESC> ACEPTAR                                                 <Ctrl-C>CANCELAR" AT 4,1 ATTRIBUTE(REVERSE)

   CALL lee_consulta() RETURNING ls_proceso

   IF ls_proceso = 1 THEN
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   CALL confirmacion() RETURNING ls_proceso
   IF ls_proceso = 1 THEN
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   CALL ejecuta_proceso()

   CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION lee_consulta()
   DEFINE ls_flag    SMALLINT

   LET ls_flag = 1
   INITIALIZE gr_captura.* TO NULL

   INPUT BY NAME gr_captura.* WITHOUT DEFAULTS
   	  AFTER FIELD fecha_ini
   	  	 IF gr_captura.fecha_ini IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR LA FECHA INICIAL"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_ini
   	  	 END IF

   	  	 IF gr_captura.fecha_fin IS NULL THEN
   	  	 	  IF gr_captura.fecha_ini > gr_captura.fecha_fin THEN
   	  	 	     ERROR "LA FECHA INICIAL NO DEBE SER MAYOR QUE LA FECHA FINAL"
   	  	 	     SLEEP 2
   	  	 	     ERROR ""
   	  	 	     NEXT FIELD fecha_ini
   	  	 	  END IF
   	  	 END IF

   	 AFTER FIELD fecha_fin
   	  	 IF gr_captura.fecha_fin IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR LA FECHA FINAL"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_fin
   	  	 END IF

   	  	 IF gr_captura.fecha_ini IS NULL THEN
   	  	 	  IF gr_captura.fecha_ini > gr_captura.fecha_fin THEN
   	  	 	     ERROR "LA FECHA INICIAL NO DEBE SER MAYOR QUE LA FECHA FINAL"
   	  	 	     SLEEP 2
   	  	 	     ERROR ""
   	  	 	     NEXT FIELD fecha_fin
   	  	 	  END IF
   	  	 END IF

      ON KEY (esc)
      	 IF gr_captura.fecha_ini IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR LA FECHA INICIAL"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_ini
   	  	 END IF

   	  	 IF gr_captura.fecha_fin IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR LA FECHA FINAL"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_fin
   	  	 END IF

   	  	 IF gr_captura.fecha_ini > gr_captura.fecha_fin THEN
   	  	    ERROR "LA FECHA INICIAL NO DEBE SER MAYOR QUE LA FECHA FINAL"
   	  	    SLEEP 2
   	  	    ERROR ""
   	  	    NEXT FIELD fecha_fin
   	  	 END IF

      	 LET ls_flag = 0
         EXIT INPUT

      ON KEY (CONTROL-C, INTERRUPT)
         LET ls_flag = 1
         EXIT INPUT
   END INPUT

   RETURN ls_flag
END FUNCTION
################################################################################
FUNCTION confirmacion()
   DEFINE ls_flag  SMALLINT
   DEFINE lc_msg   CHAR(100)
   DEFINE lc_enter CHAR(01)

   LET ls_flag = 1
   LET lc_msg  = " DESEA EJECUTAR EL PROCESO S/N? "

   WHILE TRUE
      PROMPT lc_msg CLIPPED FOR lc_enter

      IF lc_enter MATCHES "[sSnN]" THEN
         IF lc_enter MATCHES "[sS]" THEN
            LET ls_flag = 0
            EXIT WHILE
         ELSE
            LET ls_flag = 1
            EXIT WHILE
         END IF
      ELSE
         ERROR "SOLO INDIQUE S o N "
         SLEEP 2
         ERROR ""
         CONTINUE WHILE
      END IF
   END WHILE

   RETURN ls_flag
END FUNCTION
################################################################################
FUNCTION ejecuta_proceso()
   DEFINE ls_paso        SMALLINT
   DEFINE lc_comando     CHAR(500)
   DEFINE lc_enter       CHAR(001)
   DEFINE lc_asignados   CHAR(15)
   DEFINE lc_registrados CHAR(15)
   DEFINE lc_hora        CHAR(08)

   DEFINE lc_nulo CHAR(01)

   INITIALIZE lc_nulo TO NULL
   LET lc_hora = TIME

   LET gr_parametros.programa       = 'VOLB016.4gi'
   LET gr_parametros.archivo_salida = gr_seg_modulo.ruta_listados CLIPPED, "/",
                                      gc_usuario                  CLIPPED, ".",
                                      gr_captura.fecha_ini        USING "DDMMYYYY", ".",
                                      gr_captura.fecha_fin        USING "DDMMYYYY", ".",
                                      lc_hora[1,2],lc_hora[4,5],lc_hora[7,8], ".salida"

   LET gr_parametros.archivo_error  = gr_seg_modulo.ruta_listados CLIPPED, "/",
                                      gc_usuario                  CLIPPED, ".",
                                      gr_captura.fecha_ini        USING "DDMMYYYY", ".",
                                      gr_captura.fecha_fin        USING "DDMMYYYY", ".",
                                      lc_hora[1,2],lc_hora[4,5],lc_hora[7,8], ".log"

   LET lc_comando = " nohup time fglgo ",
                      gr_parametros.programa              CLIPPED,   " ",
                      gr_captura.fecha_ini                       ,   " ",
                      gr_captura.fecha_fin                       ,   " ",
                      "1> ", gr_parametros.archivo_salida CLIPPED,   " ",
                      "2> ", gr_parametros.archivo_error  CLIPPED,   " &"
   RUN lc_comando
   --DISPLAY "lc_comando: ", lc_comando CLIPPED
   --SLEEP 5

   #Desplegar datos en pantalla
   DISPLAY "PROCESO EJECUTADO POR NOHUP "                                     AT 17,1 ATTRIBUTE(REVERSE)
   DISPLAY "PROGRAMA EJECUTADO      : ", gr_parametros.programa       CLIPPED, " " AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY "VERIFIQUE ARCHIVO SALIDA: ", gr_parametros.archivo_salida CLIPPED, " " AT 19,1 ATTRIBUTE(REVERSE)
   DISPLAY "VERIFIQUE ARCHIVO LOG   : ", gr_parametros.archivo_error  CLIPPED, " " AT 20,1 ATTRIBUTE(REVERSE)

   PROMPT "<ENTER PARA CONTINUAR>" FOR CHAR lc_enter
   DISPLAY "                                                                           " AT 17,1
   DISPLAY "                                                                           " AT 18,1
   DISPLAY "                                                                           " AT 19,1
   DISPLAY "                                                                           " AT 20,1
END FUNCTION
################################################################################