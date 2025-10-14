###########################################################################
#Proyecto               => Sistema de Afores. (MEXICO)                    #
#Propietario            => E.F.P                                          #
#Programa     VOLL001   => Consulta y reporte de aportacion voluntaria    #
#Fecha                  => 29 de agosto de 2001.                          #
#Por                    => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Fecha                  => 9 de mayo de 2002.                             #
#Modificado por         => LAURA EUGENIA CORTES GUZMAN                    #
#Fecha Modif.           => 03 de Agosto del 2004                          #
#Modulo                 => VOL.                                           #
###########################################################################
DATABASE safre_af
GLOBALS

   DEFINE vfolio               INTEGER,
          vfecha_aplicacion    DATE,
          HOY                  DATE,
          usuario              CHAR(8),
          vruta_listado        CHAR(40),
          aux_pausa            CHAR(1),
          hora                 CHAR(8),
          g_lista              CHAR(100),
          g_impre              CHAR(100),
          vestado              INTEGER,
          vestado2             INTEGER

   DEFINE l_reg RECORD LIKE int_det_voluntaria.*

   DEFINE l_reg2  ARRAY[100] OF RECORD 
          folio     INTEGER,
          nss       CHAR(11),
          hora      CHAR(8),
          sucursal  SMALLINT,
          monto     DECIMAL(16,6),
          acciones  DECIMAL(16,6) 
   END RECORD
 
   DEFINE pos  SMALLINT     ,
          cla_where    CHAR(200),
          exe_sel      CHAR(300)  

   DEFINE vprecio_accion  DECIMAL(16,6),
          vacciones       DECIMAL(16,6),
          x_total_pesos   DECIMAL(16,6),
          xx_total_pesos  DECIMAL(16,6),
          x_total_acc     DECIMAL(16,6),
          xx_total_acc    DECIMAL(16,6),
          letra           CHAR(1),
          opc             CHAR(1)

END GLOBALS
#####################################################################
MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT
   LET HOY = DATE


   OPEN WINDOW ap_vol0 AT 2,3 WITH 3 ROWS , 75 COLUMNS
   ATTRIBUTE(BORDER)
       MENU "CONSULTA "
            COMMAND "Voluntaria" "Consulta Aportacion Voluntaria "
                    LET letra = "V"
                    EXIT MENU
            COMMAND "Complementaria" "Consulta Aportacion Complentaria "
                    LET letra = "C"
                    EXIT MENU
            COMMAND "Salir" "Salir "
                    LET letra = "S"
                    EXIT MENU
       END MENU
   CLOSE WINDOW ap_vol0

   IF letra = "S" THEN
      EXIT PROGRAM
   END IF

   OPEN WINDOW v1 AT 3,3 WITH FORM "VOLL0011" ATTRIBUTE (BORDER)
   DISPLAY " <ESC> Ejecutar                                           <",
	   "Ctrol-c> Salir      " AT 1,1
   IF letra = 'V' THEN
      DISPLAY " VOLL001            CONSULTA DE APORTACIONES ",
              "VOLUNTARIAS                            " 
              AT 3,1 ATTRIBUTE (REVERSE) 
   END IF

   IF letra = 'C' THEN
      DISPLAY " VOLL001          CONSULTA DE APORTACIONES ",
              "COMPLEMENTARIA                           " 
              AT 3,1 ATTRIBUTE (REVERSE) 
   END IF

   DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE (REVERSE)
   MENU "REPORTES"
      COMMAND "Pendientes" "Consulta pendiente por liquidar"
         CALL inicio()
         CALL proceso(1)
      COMMAND "Rechazadas" "Consulta de aportaciones rechazadas"
         CALL inicio()
         CALL proceso(3)
      COMMAND "Liquidadas" "Consulta de aportaciones liquidadas"
         CALL inicio()
         CALL proceso(2)
      COMMAND "Salir" "Salir de menu de consulta"
         EXIT MENU
   END MENU

   --CLOSE WINDOW v1
END MAIN
#####################################################################
FUNCTION inicio()
    SELECT USER,
           ruta_listados
    INTO   usuario,
           vruta_listado
    FROM   safre_af:seg_modulo
    WHERE  modulo_cod = "vol"

    INITIALIZE l_reg.* TO NULL
    INITIALIZE vfolio TO NULL
    INITIALIZE vfecha_aplicacion TO NULL

    LET vfolio = NULL
    LET vfecha_aplicacion = NULL
    LET vprecio_accion = NULL

    DISPLAY BY NAME vfolio
    DISPLAY BY NAME vfecha_aplicacion
##    DISPLAY "                                  ",vprecio_accion AT 05,51
    DISPLAY "" AT 05,51
    DISPLAY "               " AT 04,02
END FUNCTION
#####################################################################
FUNCTION proceso(vestado)

    DEFINE vestado        INTEGER,
           vprecio_accion DECIMAL(16,6),
           vacciones      DECIMAL(16,6)

    LET vfolio = NULL
    LET vfecha_aplicacion = NULL
    LET vprecio_accion = NULL

    LET int_flag = FALSE

    INPUT BY NAME vfolio,
                  vfecha_aplicacion WITHOUT DEFAULTS

       AFTER FIELD vfolio
          IF vfolio IS NULL THEN
             ERROR "Este campo no puede ser nulo"
             NEXT FIELD vfolio
          END IF

          SELECT "X"
          FROM   int_archivo_vol
          WHERE  folio = vfolio

          IF SQLCA.SQLCODE <> 0 THEN
             CASE letra 
	     WHEN 'V'
                PROMPT "FOLIO NO EXISTE EN APORTACION VOLUNTARIA " 
                       ATTRIBUTE (REVERSE)
                       FOR opc ATTRIBUTE (REVERSE)
                NEXT FIELD vfolio
	     WHEN 'C'
                PROMPT "FOLIO NO EXISTE EN APORTACION COMPLEMENTARIA " 
                       ATTRIBUTE (REVERSE)
                       FOR opc ATTRIBUTE (REVERSE)
                NEXT FIELD vfolio
	     END CASE
             NEXT FIELD vfecha_aplicacion
          END IF

        AFTER FIELD vfecha_aplicacion
            IF vfecha_aplicacion IS NULL THEN
                ERROR "Fecha aplicacion no puede ser nula,favor verificar."
                NEXT FIELD vfecha_aplicacion
            END IF

            SELECT precio_del_dia
            INTO   vprecio_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = vfecha_aplicacion
            AND    codigo_siefore  = 1

            IF SQLCA.SQLCODE <> 0 THEN
               PROMPT "PRECIO DE ACCION CON FECHA CAPTURADA NO EXISTE" 
                      ATTRIBUTE (REVERSE)
                      FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD vfecha_aplicacion

            END IF

            DISPLAY "Fecha Valuacion: ",vfecha_aplicacion USING "dd/mm/yyyy"
                    AT 5,2

            IF vestado = 1 OR vestado = 2 THEN
               DISPLAY "Precio accion: " ,vprecio_accion AT 05,51
            END IF

            DISPLAY "(Ctrl-c) Salir " AT 04,02

            IF vfecha_aplicacion IS NULL THEN
               ERROR "Fecha aplicacion no puede ser nula,favor verificar."
               NEXT FIELD vfecha_aplicacion
            END IF

            OPEN WINDOW ventana_2 AT 09,3 WITH FORM "VOLL0012" 
            ATTRIBUTE (BORDER)

            CASE vestado 
               WHEN  "1"
                  DISPLAY  "                          PENDIENTES POR LIQUIDAR                                       " AT 1,1 ATTRIBUTE(REVERSE)
               WHEN "2"
                  DISPLAY  "                          REGISTROS LIQUIDADOS                                       " AT 1,1 ATTRIBUTE(REVERSE)
               WHEN "3"
                  DISPLAY  "                          REGISTROS RECHAZADOS                                       " AT 1,1 ATTRIBUTE(REVERSE)

            END CASE

            DISPLAY " (Ctrl-p) Imprimi",
		    "r                                            (Ctrl-c) S",
		    "alir  " AT 2,1 
            LET int_flag = FALSE       

            IF vestado = 1 OR vestado = 2 THEN

               LET cla_where= " SELECT * ",
                              " FROM   int_det_voluntaria ",
                              " WHERE  folio =",vfolio CLIPPED,
                              " AND    estado =", vestado CLIPPED,
                              " AND    resul_operacion =","'01'",
                              " AND    tipo_vol  = '",letra,"'",
                              " ORDER BY 26 "
            ELSE 
               LET cla_where= " SELECT * ",
                              " FROM   int_det_voluntaria ",
                              " WHERE  folio =",vfolio CLIPPED,
                              " AND    resul_operacion =","'02'",
                              " AND    tipo_vol  = '",letra,"'",
                              " ORDER BY 26 "
            END IF

            PREPARE exe_sel FROM cla_where
            DECLARE cursor_1 CURSOR FOR exe_sel
                                             
            LET pos = 1
            LET x_total_pesos = 0
            LET xx_total_pesos = 0
            LET x_total_acc = 0
            LET xx_total_acc = 0

            FOREACH cursor_1 INTO l_reg.*

               LET vacciones = l_reg.monto_neto / vprecio_accion


               LET l_reg2[pos].folio     = l_reg.folio
               LET l_reg2[pos].nss       = l_reg.nss
               LET l_reg2[pos].hora      = l_reg.hora_captura[1,2],":",
                                           l_reg.hora_captura[3,4]

               LET l_reg2[pos].sucursal  = l_reg.numero_sucursal
               LET l_reg2[pos].monto     = l_reg.monto_neto
               LET l_reg2[pos].acciones  = vacciones

               LET x_total_pesos = l_reg.monto_neto

               LET xx_total_pesos = xx_total_pesos + x_total_pesos

               LET x_total_acc = vacciones

               LET xx_total_acc = xx_total_acc + x_total_acc

               LET pos = pos +1
            END FOREACH    

            DISPLAY "Total : " AT 14,28
            DISPLAY xx_total_pesos USING "#########&.&&" AT 14,41
            DISPLAY xx_total_acc   USING "#########&.&&&&&&" AT 14,60

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_reg2 TO scr_1.*
                  ON KEY (control-p)
                     ERROR "PROCESANDO IMPRESION..."
                     LET vestado2 = vestado
                     CALL listado_pendientes(vestado2)
                  ON KEY (INTERRUPT,CONTROL-C)  
                     EXIT DISPLAY
               END DISPLAY
	       DISPLAY "" AT 5,1
               CLEAR FORM
               CLOSE WINDOW ventana_2
            ELSE
               ERROR "Registros con fecha captura no existe ..."
               SLEEP 2
               ERROR ""
	       DISPLAY "" AT 5,1
               CLEAR FORM
               CLOSE WINDOW ventana_2
           END IF
           CALL inicio()
	   DISPLAY "" AT 5,1
           EXIT INPUT

       ON KEY (INTERRUPT)  
           CALL inicio()
	   DISPLAY "" AT 5,1
           EXIT INPUT
    END INPUT

    CLEAR FORM
    CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION listado_pendientes(vestado2)
   DEFINE vestado2     INTEGER

   ERROR "PROCESANDO INFORMACION ..."

   LET hora = TIME

   IF letra = 'V' THEN
      LET g_impre = vruta_listado CLIPPED,"/",usuario CLIPPED,
                    ".VOLUNTARIAS"
   END IF

   IF letra = 'C' THEN
      LET g_impre = vruta_listado CLIPPED,"/",usuario CLIPPED,
                    ".COMPLEMENTA"
   END IF

   START REPORT impresion_1 TO g_impre
      FOREACH cursor_1 INTO l_reg.*
         OUTPUT TO REPORT impresion_1(l_reg.*,vestado2,letra)
      END FOREACH
   FINISH REPORT impresion_1

   ERROR ""
   ERROR "LISTADO GENERADO ..."
   SLEEP 1
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista

END FUNCTION
###############################################################################
REPORT impresion_1(l_reg,vestado2,letra)

   DEFINE l_reg RECORD LIKE int_det_voluntaria.*

   DEFINE vestado2          INTEGER,
          letra             CHAR(1),
          x_hora            CHAR(8),
          vprecio_accion    DECIMAL(16,6),
          vacciones         DECIMAL(16,6),
          vsaldo            DECIMAL(16,6),
          vmonto_acciones   DECIMAL(16,6),
          vsaldo_accion     DECIMAL(16,6),
          documento         CHAR(11)

   DEFINE L1           CHAR(01),
          L2           CHAR(02),
          L3           CHAR(03),
          L4           CHAR(04),
          L5           CHAR(05),
          L6           CHAR(06),
          L7           CHAR(07),
          L8           CHAR(08),
          L9           CHAR(09),
          L10          CHAR(10)

  DEFINE xx_tipo_desc CHAR(20)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60

   FORMAT
      PAGE HEADER

         LET vsaldo = 0 
         LET vsaldo_accion = 0

         LET L1  = "\304"
         LET L2  = "\304\304"
         LET L3  = "\304\304\304"
         LET L4  = "\304\304\304\304"
         LET L5  = "\304\304\304\304\304"
         LET L6  = "\304\304\304\304\304\304"
         LET L7  = "\304\304\304\304\304\304\304"
         LET L8  = "\304\304\304\304\304\304\304\304"
         LET L9  = "\304\304\304\304\304\304\304\304\304"
         LET L10 = "\304\304\304\304\304\304\304\304\304\304"

         IF vestado2 = 1 OR vestado2 = 2 THEN 
            PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'

            PRINT COLUMN  01,'\033e\033(s218T\033(s13H\033(s7B'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\277'
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            IF vestado2 = 1 THEN
               PRINT COLUMN  01,"|",
                     COLUMN  02,"VOLL001";
               IF letra = 'V' THEN
                  PRINT COLUMN  35,"REPORTE DE APORTACIONES VOLUNTARIAS ",
                                   "PENDIENTES POR LIQUIDAR";
               ELSE
                  PRINT COLUMN  32,"REPORTE DE APORTACIONES COMPLEMENTARIAS ",
                                   "PENDIENTES POR LIQUIDAR";
               END IF
               PRINT COLUMN 120,hoy USING "DD-MM-YYYY",
                     COLUMN 135,"|"
            ELSE
               PRINT COLUMN  01,"|",
                     COLUMN  02,"VOLL001";
               IF letra = 'V' THEN
                  PRINT COLUMN  38,"REPORTE DE APORTACIONES VOLUNTARIAS ",
                                   "LIQUIDADAS    ";
               ELSE
                  PRINT COLUMN  35,"REPORTE DE APORTACIONES COMPLEMENTARIAS ",
                                   "LIQUIDADAS    ";
               END IF
               PRINT COLUMN 120,hoy USING "DD-MM-YYYY",
                     COLUMN 135,"|"
            END IF


            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\277'

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN  02,"FOLIO",
                  COLUMN  14,"NSS",
                  COLUMN  24,"F. APLICACION",
                  COLUMN  40,"HORA",
                  COLUMN  50,"SUC.",
                  COLUMN  58,"IMPORTE",
                  COLUMN  73,"SALDO(+)",
                  COLUMN  84,"PRECIO ACC.",
                  COLUMN 102,"ACCIONES",
                  COLUMN 118,"SALDO ACC.(+)",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\277'
         ELSE
            PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'

            PRINT COLUMN  01,'\033e\033(s218T\033(s13H\033(s7B'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,       
                                    L10,L10,L10,L8,'\277'
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN  02,"VOLL001";
            IF letra = 'V' THEN
               PRINT COLUMN  38,"REPORTE DE APORTACIONES VOLUNTARIAS ",
                                "RECHAZADAS    ";
            ELSE
               PRINT COLUMN  35,"REPORTE DE APORTACIONES COMPLEMENTARIAS ",
                                "RECHAZADAS    ";
            END IF
            PRINT COLUMN 120,hoy USING "DD-MM-YYYY",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\277'
            PRINT COLUMN  01,"|",
                 COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN  02,"FOLIO",
                  COLUMN  14,"NSS",
                  COLUMN  24,"F. APLICACION",
                  COLUMN  40,"HORA",
                  COLUMN  50,"SUC.",
                  COLUMN  58,"IMPORTE",
                  COLUMN  73,"SALDO(+)",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\277'
         END IF 

      ON EVERY ROW
         LET x_hora = l_reg.hora_captura[1,2],":",
                      l_reg.hora_captura[3,4]

         IF vestado2 = 1 OR vestado2 = 2 THEN

            SELECT precio_del_dia
            INTO   vprecio_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = vfecha_aplicacion
            AND    codigo_siefore  = 1

            LET vsaldo = l_reg.monto_neto + vsaldo

            LET vmonto_acciones = l_reg.monto_neto / vprecio_accion

            LET vsaldo_accion = vmonto_acciones + vsaldo_accion

            PRINT COLUMN 01,"|",
                  COLUMN 02,l_reg.folio USING "<<<<<<",
                  COLUMN 10,l_reg.nss,
                  COLUMN 26,vfecha_aplicacion USING "DD/MM/YYYY",
                  COLUMN 40,x_hora,
                  COLUMN 48,l_reg.numero_sucursal,
                  COLUMN 56,l_reg.monto_neto USING "########.##",
                  COLUMN 70,vsaldo USING "########.##",
                  COLUMN 84,vprecio_accion USING "#&.&&&&&&",
                  COLUMN 97,vmonto_acciones USING "########&.&&&&&&",
                  COLUMN 115,vsaldo_accion USING "########&.&&&&&&",
                  COLUMN 135,"|"

            PRINT COLUMN 01,"|",
                  COLUMN 135,"|"
         ELSE
            LET vsaldo = l_reg.monto_neto + vsaldo

            PRINT COLUMN 01,"|",
                  COLUMN 02,l_reg.folio USING "<<<<<<",
                  COLUMN 10,l_reg.nss,
                  COLUMN 26,vfecha_aplicacion USING "DD/MM/YYYY",
                  COLUMN 40,x_hora,
                  COLUMN 48,l_reg.numero_sucursal,
                  COLUMN 56,l_reg.monto_neto USING "########.##",
                  COLUMN 70,vsaldo USING "########.##",
                  COLUMN 135,"|"

            PRINT COLUMN 01,"|",
                  COLUMN 135,"|"
         END IF

      ON LAST ROW
         PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                 L10,L10,L10,L8,'\331'
         SKIP 2 LINE
         PRINT COLUMN 01," TRANSACCIONES: ",COUNT(*) USING "<<<<"

END REPORT
