#s############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.       					    #
#Programa          => GENERACION DE ESTADISTICA APORT.VOL.CONSAR 19-5       #
#                     (formato 25)                                          # 
#Fecha             => 19 junio 2006                                         #
#Por               => JESUS DAVID YAÑEZ MORENO                              #
#############################################################################
DATABASE safre_af

GLOBALS
DEFINE bn                 SMALLINT
DEFINE num_regs           INTEGER   ,
       cuantos1           INTEGER   ,
       cuantos2           INTEGER   ,
       cuantos3           INTEGER   

define pso                CHAR(500)  ,
       txt                CHAR(1500) ,
       x_busca            CHAR(100)  ,
       nom_dis_cuenta     CHAR(020)  ,
       usuario            CHAR(010)  ,
       vecho              CHAR(400)  ,
       enter              CHAR(001)  

define sw                 SMALLINT

DEFINE fecha_retro1       DATE

DEFINE mont1              DECIMAL(16,6),
       mont2              DECIMAL(16,6),
       mont3              DECIMAL(16,6)

DEFINE g_reg  RECORD
    fecha_arch         DATE
END RECORD
    
        DEFINE g_cuentas       RECORD 
             nss               CHAR(011) ,
             tipo_aportacion   CHAR(002) ,
             tipo_operacion    CHAR(002) , 
             tipo_deposito     CHAR(002) ,
             fecha_recepcion   DATE      ,
             fecha_abono       DATE      ,
             fecha_inversion   DATE      ,
             tipo_siefore      CHAR(003) ,
             clave_siefore     CHAR(003) ,
             subtipo_siefore   CHAR(003) ,
             curp              CHAR(018) ,
             rfc               CHAR(013) ,
             nombre            CHAR(020) ,
             paterno           CHAR(020) ,
             materno           CHAR(020) ,
             fecha_nacimiento  DATE      ,
             monto_aportacion  DEC(16,2) ,
             monto_acciones    DEC(16,6) ,
             folio             INTEGER
        END RECORD
     
        DEFINE gg              RECORD
             impt_aportacion      DECIMAL(16,6)
        END RECORD

        DEFINE g_tabafore      RECORD LIKE tab_afore_local.*

        DEFINE 
            HOY                 DATE       ,
	    G_LISTA	       	CHAR(500)  ,
	    G_LISTAP	       	CHAR(500)  ,
	    G_LISTAr	       	CHAR(500)  ,
	    aux_pausa       	CHAR(1)    

	DEFINE g_paramgrales   	RECORD LIKE seg_modulo.*

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("ESTM006.log")

    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "ESTM0061" ATTRIBUTE(BORDER)
    DISPLAY "ESTM006       ESTADISTICA DE APORTACIONES VOLUNTARIAS                                   " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                                 < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    CALL init()

LET sw = 0 

    LET int_flag = FALSE 
    CONSTRUCT BY NAME x_busca ON fecha_conversion 
    ON KEY (ESC)
       LET int_flag = FALSE
       EXIT CONSTRUCT 
    ON KEY (INTERRUPT)
       LET int_flag = FALSE
       EXIT PROGRAM
    END CONSTRUCT 

    INPUT BY NAME g_reg.*

      AFTER FIELD fecha_arch

      IF g_reg.fecha_arch IS NULL THEN
        DISPLAY  "Campo NO puede ser NULO" AT 19,1
        NEXT FIELD fecha_arch
      END IF

    ON KEY(ESC)
      IF (g_reg.fecha_arch IS NOT  NULL   AND 
          g_reg.fecha_arch >= '05/11/2006'   ) THEN
         EXIT INPUT
      ELSE 
        ERROR "FECHA NO VALIDA ..."
        NEXT FIELD fecha_arch
      END IF

    ON KEY ( INTERRUPT )
          ERROR "Proceso Cancelado ...." 
          SLEEP 2
          EXIT PROGRAM
    END INPUT 

    CALL query() 

    DISPLAY "ESTADISTICA GENERADA EN : "  AT 17,2
    LET G_LISTA = G_LISTA[11,500] CLIPPED
    DISPLAY G_LISTA AT 18,2
 
    PROMPT "<ENTER> Para Salir ..."  FOR char enter

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END MAIN

FUNCTION init()
         LET bn = 0 
         LET num_regs = 0

         SELECT user
         INTO usuario
         FROM tab_afore_local
         group by 1

         SELECT * 
         INTO g_paramgrales.*
         FROM seg_modulo
	 WHERE modulo_cod = "est"

     SELECT codigo_afore
     INTO   g_tabafore.codigo_afore
     FROM   tab_afore_local

     CREATE TEMP TABLE est_ascii(simbolo char(01))

INSERT into est_ascii VALUES ("[")
INSERT INTO est_ascii VALUES ('"')
INSERT INTO est_ascii VALUES ("]")
INSERT INTO est_ascii VALUES ("#")
INSERT INTO est_ascii VALUES ("$")
INSERT INTO est_ascii VALUES ("%")
INSERT INTO est_ascii VALUES ("&")
INSERT INTO est_ascii VALUES ("=") 
INSERT INTO est_ascii VALUES ("/")
INSERT INTO est_ascii VALUES ("?")
INSERT INTO est_ascii VALUES ("-")
INSERT INTO est_ascii VALUES ("'")
INSERT INTO est_ascii VALUES ("(")
INSERT INTO est_ascii VALUES (")")
INSERT INTO est_ascii VALUES ("^")
INSERT INTO est_ascii VALUES ("!") 
INSERT INTO est_ascii VALUES ("~")
INSERT INTO est_ascii VALUES ("_")
INSERT INTO est_ascii VALUES (".")
INSERT INTO est_ascii VALUES (":")
INSERT INTO est_ascii VALUES (",")
INSERT INTO est_ascii VALUES (";")
INSERT INTO est_ascii VALUES ("<")
INSERT INTO est_ascii VALUES (">") 
INSERT INTO est_ascii VALUES ("@")
INSERT INTO est_ascii VALUES ("|")
INSERT INTO est_ascii VALUES ("{")
INSERT INTO est_ascii VALUES ("}")
INSERT INTO est_ascii VALUES ("+")
INSERT INTO est_ascii VALUES ("*")
INSERT INTO est_ascii VALUES ("`")
INSERT INTO est_ascii VALUES ("¿") 
INSERT INTO est_ascii VALUES ("¡")
INSERT INTO est_ascii VALUES ("Ä")
INSERT INTO est_ascii VALUES ("É")
INSERT INTO est_ascii VALUES ("Í")
INSERT INTO est_ascii VALUES ("Ó")
INSERT INTO est_ascii VALUES ("Ú")
INSERT INTO est_ascii VALUES ("¨")
INSERT INTO est_ascii VALUES ("Ä") 
INSERT INTO est_ascii VALUES ("Ë")
INSERT INTO est_ascii VALUES ("Ï")
INSERT INTO est_ascii VALUES ("Ö")
INSERT INTO est_ascii VALUES ("Ö")
INSERT INTO est_ascii VALUES ("Ü")
INSERT INTO est_ascii VALUES ("á")
INSERT INTO est_ascii VALUES ("é")
INSERT INTO est_ascii VALUES ("í") 
INSERT INTO est_ascii VALUES ("ó")
INSERT INTO est_ascii VALUES ("ú")
INSERT INTO est_ascii VALUES ("ä")
INSERT INTO est_ascii VALUES ("ë")
INSERT INTO est_ascii VALUES ("ï")
INSERT INTO est_ascii VALUES ("ö")
INSERT INTO est_ascii VALUES ("ü")
INSERT INTO est_ascii VALUES ("´") 
INSERT INTO est_ascii VALUES ("Á")
INSERT INTO est_ascii VALUES ("Ð")



END FUNCTION


FUNCTION query()
#q-------------

DEFINE g_qry1 CHAR(600)

  LET g_qry1 = ' SELECT UNIQUE  ',
               ' "01" , ',
             --  ' "00" , ',
             --' CASE WHEN b.medio = 1 THEN "02" ',
             --'      WHEN b.medio = 2 THEN "01" ',
             --'      WHEN b.medio = 3 THEN "02" ',
             --'      ELSE "09"        ',
             --' END CASE , ', 
             ' CASE WHEN b.forma_pago = 1 THEN "00" ',
             '      ELSE "01" ',
             ' END CASE , ', 
               ' b.fecha , ',
               ' b.fecha   ',
               ' FROM int_det_voluntaria b ',
               ' WHERE  b.folio   = ? ',   # reg_cuentas.folio
               ' AND    b.nss     = ? ',   # reg_cuentas.nss
               ' AND    b.monto_neto  = ? ',   # reg_cuentas.monto_aportacion
               ' AND    resul_operacion = "01" ',  # procedentes
               ' AND    fecha_liquida = ? '  # fecha_liquidacion

  PREPARE qry1 FROM g_qry1 
  DECLARE cur_qry1 CURSOR FOR qry1
  LET G_LISTAP =g_paramgrales.ruta_envio CLIPPED,
              "/",g_reg.fecha_arch USING "yyyymmdd" CLIPPED,
              "_AF_",g_tabafore.codigo_afore USING"&&&","_000",
              ".0502p" CLIPPED

  LET G_LISTA =g_paramgrales.ruta_envio CLIPPED,
              "/",g_reg.fecha_arch USING "yyyymmdd" CLIPPED,
              "_AF_",g_tabafore.codigo_afore USING"&&&","_000",
              ".0502" CLIPPED

  --LET G_LISTAr =g_paramgrales.ruta_listados CLIPPED,
              --"/",usuario CLIPPED, 
              --".REP",g_reg.fecha_arch USING "yyyymmdd" CLIPPED,
              --".068n" CLIPPED

  START REPORT listado TO G_LISTAP
--START REPORT listador TO G_LISTAr

PROMPT " Deseas Generar el Archivo  S/N  ?  " for CHAR aux_pausa
 
  DISPLAY "Procesando Informacion ...."  AT 20,1 ATTRIBUTE(REVERSE)

IF aux_pausa MATCHES "[Ss]" THEN

     DECLARE cur_1 CURSOR FOR 
     SELECT a.nombre_tabla  
     FROM   taa_cd_tab_cuenta a

     FOREACH cur_1 INTO nom_dis_cuenta 

     LET txt = 
      ' SELECT a.nss                          , ',     #nss
      '  CASE WHEN a.subcuenta = 10 THEN "05"   ',     #tipo_aportacion
      '   WHEN a.subcuenta = 12 THEN "06"       ',     
      '   WHEN a.subcuenta = 15 THEN "07"       ',
      '   WHEN a.subcuenta = 16 THEN "07"       ',#MOD 210307 APOR_LARGO_VENT
      '  END CASE         , ',
      '  ""             , ',                           #tipo_operacion
      '  ""             , ',                           #tipo_deposito
      '  ""               , ',                         #fecha_recepcion
      '  ""               , ',                         #fecha_abono
      '  a.fecha_conversion                   , ',     #fecha_inversion
      '  CASE WHEN a.siefore = 1 THEN "002"     ',     #tipo_siefore
      '   WHEN a.siefore = 2 THEN "002"         ',     
      '   WHEN a.siefore = 3 THEN "002"         ',     -- Antes 017
      '   WHEN a.siefore = 4 THEN "002"         ',     
      '   WHEN a.siefore = 5 THEN "002"         ',     
      '  END CASE           , ',
      '  " "                , ',                       #clave_siefore
      '  CASE WHEN a.siefore = 1 THEN "001"  ',        #subtipo_siefore
      '   WHEN a.siefore = 2 THEN "002"      ',
      '   WHEN a.siefore = 3 THEN "003"      ',
      '   WHEN a.siefore = 4 THEN "004"      ',
      '   WHEN a.siefore = 5 THEN "005"      ',
      '  END CASE    , ',
      '  c.n_unico   , ',                              #curp
      '  c.n_rfc     , ',                              #rfc
      '  c.nombres   , ',                              #nombre
      '  c.paterno   , ',                              #paterno
      '  c.materno   , ',                              #materno
      '  c.fena      , ',                              #fecha_nacimiento
      '  a.monto_en_pesos  , ',                        #monto_aportacion
      '  a.monto_en_acciones,    ',                    #monto_acciones
      '  a.folio                ',                     #folio dis_cuenta
      ' FROM  ',nom_dis_cuenta CLIPPED,' a, ',
      '   afi_mae_afiliado   c     ',
      ' WHERE ', x_busca    CLIPPED  ,
      ' AND   a.tipo_movimiento = 1       ',
      ' AND   a.subcuenta       IN (10,12,15,16) ',#MOD 210307 APOR_LARGO_VNT16
      ' AND   a.nss             = c.n_seguro   ',
      ' AND   a.id_aportante    matches "VE*"  '

      PREPARE qry FROM txt

      DECLARE cur_2 CURSOR FOR qry 
      FOREACH cur_2 INTO  g_cuentas.*
   
      LET g_cuentas.nombre = formato_nombre(g_cuentas.nombre) 
      LET g_cuentas.paterno= formato_nombre(g_cuentas.paterno) 
      LET g_cuentas.materno= formato_nombre(g_cuentas.materno) 

      IF g_cuentas.fecha_inversion < '01/14/2005' THEN 
         LET g_cuentas.subtipo_siefore = "001"
      END IF

      CASE g_cuentas.tipo_siefore 
      WHEN "002" 
         LET g_cuentas.clave_siefore = g_tabafore.codigo_afore
         EXIT CASE
      WHEN "017" 
         LET g_cuentas.clave_siefore = g_tabafore.codigo_afore + 100
         EXIT CASE
      END CASE

      FOREACH cur_qry1 USING g_cuentas.folio  ,
                         g_cuentas.nss    ,
                         g_cuentas.monto_aportacion,
                         g_cuentas.fecha_inversion
                   INTO  g_cuentas.tipo_operacion ,
                         g_cuentas.tipo_deposito  ,
                         g_cuentas.fecha_recepcion,
                         g_cuentas.fecha_abono 

          EXIT FOREACH
      END FOREACH
      

      IF (g_cuentas.fecha_recepcion IS NULL OR 
          g_cuentas.fecha_recepcion = ""   ) THEN

          LET g_cuentas.tipo_operacion = "01"
          LET g_cuentas.tipo_deposito  = "00"
          LET g_cuentas.fecha_recepcion = 
          habil_anterior(g_cuentas.fecha_inversion) 
          LET g_cuentas.fecha_abono     = g_cuentas.fecha_recepcion

      END IF

         LET g_cuentas.fecha_abono = 
         habil_anterior(g_cuentas.fecha_inversion)

         LET num_regs = num_regs + 1

         OUTPUT TO REPORT listado()
       --OUTPUT TO REPORT listador()
                             
      END FOREACH

    END FOREACH

    IF num_regs = 0 THEN 
         LET num_regs = 1
         LET bn       = 1 
         OUTPUT TO REPORT listado()
    END IF 

    FINISH REPORT listado
  --FINISH REPORT listador
        IF num_regs > 1 THEN
           LET num_regs = num_regs + 1
           LET pso = "sed 's/24300001/243",num_regs USING"&&&&&","/g' ",
                     G_LISTAP CLIPPED ," > ",G_LISTA
        ELSE
            IF bn = 1 THEN
               LET pso = "grep '^000' ",G_LISTAP CLIPPED,
                          " > ",G_LISTA CLIPPED
               LET bn = 0
            ELSE
               LET num_regs = num_regs + 1
               LET pso = "sed 's/24300001/243",num_regs USING"&&&&&","/g' ",
                         G_LISTAP CLIPPED ," > ",G_LISTA
            END IF
         END IF

         RUN pso 

         LET pso = 'rm ', G_LISTAP

         RUN pso

         LET G_LISTA ="chmod 777 ",g_paramgrales.ruta_envio CLIPPED, 
                       "/",g_reg.fecha_arch USING "yyyymmdd" CLIPPED,
                       "_AF_",g_tabafore.codigo_afore USING"&&&","_000",
		       ".0502" CLIPPED
         RUN G_LISTA

         --LET G_LISTAr ="chmod 777 ",g_paramgrales.ruta_envio CLIPPED, 
                       --"/rep",g_reg.fecha_arch USING "yyyymmdd" CLIPPED,
		       --".068n" CLIPPED
         --RUN G_LISTAr

	 LET vecho = "echo ",g_reg.fecha_arch USING"YYYYMMDD" CLIPPED,
                     "_AF_",g_tabafore.codigo_afore USING"&&&","_000",
		     ".0502" CLIPPED,
		     " > ",g_paramgrales.ruta_envio CLIPPED,
		     "/rescate.0502"
         RUN vecho
		       
         --LET G_LISTAr ="lp ",g_paramgrales.ruta_envio CLIPPED, 
                       --"/rep",g_reg.fecha_arch USING "yyyymmdd" CLIPPED,
		       --".068n" CLIPPED
        -- RUN G_LISTAr
      ELSE 
          ERROR "PROCESO CANCELADO ..."
          SLEEP 2
          EXIT PROGRAM 
      END IF

END FUNCTION

REPORT listado()

   OUTPUT
  PAGE LENGTH 100000
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

   FORMAT
   PAGE HEADER
       PRINT COLUMN 01,"000"                              ,
             COLUMN 04,"0502"                             ,
             COLUMN 08,"001"                              ,
             COLUMN 11,g_tabafore.codigo_afore USING"&&&" ,
             COLUMN 14,g_reg.fecha_arch USING"YYYYMMDD"   ,
             COLUMN 22,"243"                              ,
             COLUMN 25,num_regs USING"&&&&&"              ,
             COLUMN 30,214 SPACES
 
    ON EVERY ROW
       IF g_cuentas.nss[1] = "I" THEN 
          LET g_cuentas.nss = "           "
       END IF
       PRINT COLUMN 01,"301"                                       ,
             COLUMN 04,g_cuentas.tipo_aportacion                   ,
             COLUMN 06,g_cuentas.tipo_operacion                    ,
             COLUMN 08,g_cuentas.tipo_deposito                     ,
             COLUMN 10,g_cuentas.fecha_recepcion USING"YYYYMMDD"   ,
             COLUMN 18,g_cuentas.fecha_abono     USING"YYYYMMDD"   ,
             COLUMN 26,g_cuentas.fecha_inversion USING"YYYYMMDD"   ,   
             COLUMN 34,g_cuentas.tipo_siefore                      ,
             COLUMN 37,g_cuentas.clave_siefore                     ,
             COLUMN 40,g_cuentas.subtipo_siefore                   ,
             COLUMN 43,g_cuentas.nss                               ,
             COLUMN 54,g_cuentas.curp                              ,
             COLUMN 72,g_cuentas.rfc                               ,
             COLUMN 85,g_cuentas.nombre                            ,
             COLUMN 125,g_cuentas.paterno                          ,
             COLUMN 165,g_cuentas.materno                          ,
             COLUMN 205,g_cuentas.fecha_nacimiento USING"YYYYMMDD" ,
             COLUMN 213,g_cuentas.monto_aportacion * 100 
                     USING"&&&&&&&&&&&&&&&&"                ,   
             COLUMN 229,g_cuentas.monto_acciones   * 1000000
                     USING"&&&&&&&&&&&&&&&"             
END REPORT
{
REPORT listador()
#lr-------------

    DEFINE 
         tipo_de_reg          CHAR(3),
         num_de_reg           CHAR(5),
         tama_de_reg          CHAR(3),
         tipo_de_arc          CHAR(3),
         entidad              CHAR(3),
         fecha_envio          DATE,
         esp_en_blan          CHAR(34)

    DEFINE
         tipo_de_regi         CHAR(3),
         fecha_recepcion      DATE,
         num_aportaciones     INTEGER,
         tipo_de_oper         CHAR(2),
         monto_inversion     DECIMAL(16,2),
         fecha_inversion      DATE,
         monto_aportacion     DECIMAL(16,2),
         monto_inver          DECIMAL(16,2)

   OUTPUT
      TOP MARGIN  0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90
   FORMAT

    PAGE HEADER 

    PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
    PRINT COLUMN 001,"==================================================",
                     "==================================================",
                     "==================================================",
                     "============="
    PRINT
    PRINT COLUMN 001, "AFORE ACTINVER"
    PRINT COLUMN 001, "ESTM006"
    PRINT COLUMN 001, TODAY USING "DD-MM-YYYY" 
    PRINT COLUMN 001, "ESTADISTICA ARCHIVO (068n) APORTACIONES VOLUNTARIAS"
    PRINT COLUMN 001, "FECHA ENVIO: ", g_reg.fecha_arch USING "DD-MM-YYYY"
    PRINT 
    PRINT COLUMN 001,"==================================================",
                     "==================================================",
                     "==================================================",
                     "============="

    PRINT

     ON EVERY ROW

       LET tipo_de_regi     = "301" 
       LET num_aportaciones = g_cuentas.total     
       LET tipo_de_oper     = "01"

       IF gg.impt_aportacion IS NULL THEN 
          LET monto_inversion = "0000000000000000"
       ELSE 
          LET monto_inversion = gg.impt_aportacion
       END IF


       IF g_cuentas.monto_en_pesos IS NULL THEN
          LET monto_aportacion = "0000000000000000"
       ELSE
          LET monto_aportacion  = g_cuentas.monto_en_pesos   
       END IF

       IF g_cuentas.monto_inver IS NULL THEN
          LET monto_inver = "0000000000000000"
       ELSE
          LET monto_inver = g_cuentas.monto_inver
       END IF


       PRINT COLUMN 01,"FECHA RECEPCION"   ,
             COLUMN 21,"T. APORTACIONES"   ,
             COLUMN 38,"T. OPERACION"      ,
             COLUMN 52,"           MONTO"  ,
             COLUMN 70,"FECHA INVERSION"   ,
             COLUMN 87,"           MONTO" 

       PRINT COLUMN 06,fecha_recepcion USING "DD-MM-YYYY",
             COLUMN 30,num_aportaciones USING "#####&",
             COLUMN 48,tipo_de_oper  CLIPPED,
             COLUMN 52,monto_aportacion USING "############&.&&",
             COLUMN 75,fecha_inversion  USING "DD-MM-YYYY",
             COLUMN 87,monto_inver USING "############&.&&" 

    PRINT
    PRINT COLUMN 001,"==================================================",
                     "==================================================",
                     "==================================================",
                     "============="
           
END REPORT
}
##################################################################################
FUNCTION cal_fecha_retro(x_fecha,ciclo)
#cf-------------------------------

    DEFINE cc         SMALLINT
    DEFINE x_fecha    DATE
    DEFINE ciclo      SMALLINT

    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT

    LET sig_fecha  = x_fecha
    LET cc = 1

    WHILE cc <= ciclo 

        LET sig_fecha  = sig_fecha - 1

        LET dia_semana = WEEKDAY(sig_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
           CONTINUE WHILE
        ELSE  
           SELECT "ok" 
           FROM   tab_feriado
           WHERE  feria_fecha = sig_fecha
       
           IF STATUS <> NOTFOUND THEN
              CONTINUE WHILE
           ELSE 
	      LET cc = cc + 1
           END IF	
        END IF
    END WHILE
    RETURN sig_fecha
END FUNCTION

FUNCTION habil_anterior(diaActual)

   DEFINE diaTmp          DATE,
          contador        SMALLINT,
          diaActual       DATE,
          diaHabilAnt     DATE,
          diaSemana       SMALLINT,
          feriado         SMALLINT,
          finSemana       SMALLINT

   LET diaHabilAnt = diaActual - 1 UNITS DAY

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilAnt)

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado
      WHERE  feria_fecha = diaHabilAnt

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
          LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilAnt

END FUNCTION #habil_anterior

FUNCTION formato_nombre(cadena)
DEFINE cadena CHAR(40)
DEFINE in SMALLINT
DEFINE cad_in CHAR(001)

       FOR in = 1 TO LENGTH(cadena)
  
              LET cad_in = cadena[in]

              SELECT "OK"
              FROM   est_ascii a 
              WHERE  a.simbolo = cad_in

              IF STATUS <> NOTFOUND THEN
               LET cadena[in] = 'Ñ'
              END IF
       END FOR
RETURN cadena

END FUNCTION
