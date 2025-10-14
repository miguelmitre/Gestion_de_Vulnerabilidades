################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Por               => E.F.P.                                                   #
#Programa  INTB0902=> CARGA DE ARCHIVO DE VALOR DE LAS UDI's                   #
#Fecha             => 21-Noviembre-2008                                        #
#Por               => Isabel Fonseca Frias                                     #
#Sistema           => INT                                                      #
################################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_param  RECORD       LIKE seg_modulo.*


   DEFINE reg RECORD #glo 
          codigo_siefore        LIKE glo_valor_accion.codigo_siefore ,
          precio_del_dia        LIKE glo_valor_accion.precio_del_dia,
          fecha_valuacion       LIKE glo_valor_accion.fecha_valuacion 
   END RECORD


   DEFINE HOY                   DATE

   DEFINE vafore_local          SMALLINT,
          vcodigo_siefore       SMALLINT


   DEFINE n_registros           CHAR(165),
          comando               CHAR(200),
          generar               CHAR(18),
          cuantos               INTEGER,
          cuantos_1             INTEGER,
          cuantos_2             INTEGER,
          c_fecha_operacion     CHAR(10),
          c_fecha_valuacion     CHAR(10),
          opc                   CHAR(1),
          aux_pausa             CHAR(1)

END GLOBALS
#####################################################################
MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   LET HOY = TODAY

   -- Obtiene el codigo de la AFORE con la que se esta trabajando

    LET vafore_local = NULL

    SELECT e.codigo_afore
      INTO vafore_local
      FROM tab_afore_local e

   CALL ventana()

   CALL proceso()

      PROMPT "PROCESO FINALIZO NORMALMENTE.... PRESIONE < ENTER > PARA SALIR"
      FOR CHAR aux_pausa
   CLOSE WINDOW ventana_1

END MAIN
#####################################################################
FUNCTION ventana()

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "INTB09021" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                           < ",
           "Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " INTB0902   CARGA DE ARCHIVO DE VALOR DE LAS UDI's ",
           "SIEFORE 13                 " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION
#####################################################################
FUNCTION PROCESO()

   DEFINE dia           DATE,
          dia_valuacion DATE,
          enter         CHAR(1)


   SELECT ruta_rescate
   INTO   g_param.ruta_rescate
   FROM   seg_modulo
   WHERE  modulo_cod = "int"

   LET INT_FLAG = FALSE

   INPUT BY NAME generar

      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
         END IF

      ON KEY (ESC)
         LET comando = "cd ",g_param.ruta_rescate CLIPPED,
                       "; ls > archivos_13" CLIPPED
         RUN comando

         WHENEVER ERROR CONTINUE
         DROP TABLE archivos
         WHENEVER ERROR STOP

         CREATE TEMP TABLE archivos
            (campo CHAR(250))

         LET comando = g_param.ruta_rescate CLIPPED,"/archivos_13" CLIPPED

         LOAD FROM comando INSERT INTO archivos

         LET comando = "cd ",g_param.ruta_rescate CLIPPED,
                       "rm archivos_13" CLIPPED
         RUN comando

         SELECT "X"
         FROM   archivos
         WHERE  campo = generar

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"

            WHENEVER ERROR CONTINUE
            DROP TABLE archivos 
            WHENEVER ERROR STOP

            NEXT FIELD generar

         ELSE

             WHENEVER ERROR CONTINUE
             DROP TABLE carga_valor
             WHENEVER ERROR STOP

       
            CREATE TEMP TABLE carga_valor
               (campo CHAR(50))
   
               LET comando = g_param.ruta_rescate CLIPPED,"/",generar CLIPPED
   
   
               LOAD FROM comando INSERT INTO carga_valor 
   
               EXIT INPUT
         END IF
      ON KEY (CONTROL-C,INTERRUPT)
         ERROR "PROCESO CANCELADO..."
         SLEEP 2
         ERROR ""
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLEAR SCREEN
      EXIT PROGRAM
   END IF

   LET cuantos = 0


   SELECT COUNT(*)
   INTO cuantos 
   FROM   carga_valor

issss
   DISPLAY "REGISTROS A INGRESAR    ........", cuantos AT 15,10

   ERROR "PROCESANDO INFORMACION "

   DECLARE cur_1 CURSOR FOR
   SELECT *
   FROM   carga_valor 


   LET cuantos_1 = 0
   LET cuantos_2 = 0

   FOREACH cur_1 INTO  n_registros
     
      LET reg.precio_del_dia     =  n_registros [001,020]
      LET c_fecha_valuacion      =  n_registros [025,026],"/",
                                    n_registros [027,028],"/" ,
                                    n_registros [021,024]
      LET reg.fecha_valuacion    =  c_fecha_valuacion


         SELECT "a.X" 
           FROM glo_valor_accion a
         WHERE  a.fecha_valuacion = reg.fecha_valuacion
         AND    a.codigo_siefore  = 13 

         IF SQLCA.SQLCODE = 0 THEN

            LET cuantos_2 = cuantos_2 + 1
         ELSE
      
            LET cuantos_1 = cuantos_1 + 1

                  INSERT INTO glo_valor_accion
                  VALUES (13                   ,
                          reg.precio_del_dia   ,      
                          " "                  ,
                          reg.fecha_valuacion  ,   
                          0                    ,
                          0                    ,
                          0                    ,
                          0                    ,
                          0                    ,
                          0                    ,
                          0                    ,
                          0)

         END IF


   END FOREACH
      
   DISPLAY "REGISTROS INGRESADOS    ........", cuantos_1 AT 16,10
   DISPLAY "REGISTROS NO INGRESADOS ........", cuantos_2 AT 17,10

END FUNCTION
#####################################################################
