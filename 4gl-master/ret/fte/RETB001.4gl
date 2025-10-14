###############################################################################
#Propietario       => E.F.P.                                                  #
#Programa          => GENERA ARCHIVO DE PRECIO DE ACCION POR SIEFORE          #
#Fecha             => 15 diciembre 2004                                       #
#Realizado         => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha             => 15 dic 2004                                             #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE enter    CHAR(001)
    DEFINE reg_2    RECORD LIKE glo_valor_accion.*

    DEFINE 
        HOY               DATE,
        vfecha            DATE,
        G_LISTA	          CHAR(100),
        G_LISTA1          CHAR(100),
        G_LISTA2          CHAR(100),
        G_LISTA3          CHAR(100),
        cat               CHAR(500),
        borra             CHAR(200),
        aux_pausa         CHAR(1),
        char              CHAR(1),
        vcodigo_afore     CHAR(3),
        cont              INTEGER,
        vfolio            INTEGER    

    DEFINE g_paramgrales  RECORD LIKE seg_modulo.*
    DEFINE cla_sel        CHAR(100)
    DEFINE ejecuta        CHAR(100)
END GLOBALS


MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL inicio()  #i
    CALL proceso_principal()  #pp
END MAIN


FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT codigo_afore
    INTO   vcodigo_afore
    FROM   tab_afore_local
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "RETB0011" ATTRIBUTE(BORDER)
    DISPLAY "RETB001             GENERA ARCHIVO DE PRECIOS DE ACCION                             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "    [ Esc ] Iniciar                                       [ Ctrl-C ] Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfecha

      AFTER FIELD vfecha
          SELECT "X"
          FROM   glo_valor_accion
          WHERE  fecha_valuacion = vfecha
          AND    codigo_siefore  = 1
          GROUP BY 1

          IF STATUS = NOTFOUND THEN
             ERROR "NO EXISTE VALOR PARA ESTE DIA"
	          SLEEP 2
	          NEXT FIELD vfecha
          END IF

      ON KEY (ESC)
          ERROR " PROCESANDO INFORMACION " 

          CALL genera_reporte()   #gr

      ERROR " "
      DISPLAY "ARCHIVO GENERADO EN : ",G_LISTA3 AT 16,1
      PROMPT "PROCESO FINALIZADO ...[ENTER] PARA CONTINUAR "
      FOR enter

      EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM

    END INPUT

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1
END FUNCTION


FUNCTION genera_reporte()
#gr----------------------
    DEFINE vrazon_siefore CHAR(8)
    DEFINE vprecio        DECIMAL(16,6)
    DEFINE vcodigo        SMALLINT

    SELECT * 
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                  vfecha USING"YYYYMMDD",".PA.D"

    LET G_LISTA1 = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                  vfecha USING"YYYYMMDD",".PA.E"
    LET G_LISTA2 = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                  vfecha USING"YYYYMMDD",".PA.S"
    LET G_LISTA3 = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                  vfecha USING"YYYYMMDD",".PA"
    LET cont  = 0

    START REPORT listado_2 TO G_LISTA
    START REPORT listado_3 TO G_LISTA1
    START REPORT listado_4 TO G_LISTA2
    DECLARE cur_2 CURSOR FOR
       SELECT codigo_siefore,
              precio_del_dia
       FROM   glo_valor_accion
       WHERE  fecha_valuacion = vfecha
       AND    codigo_siefore not in(0,11)
       ORDER BY codigo_siefore

    FOREACH cur_2 INTO vcodigo,vprecio
       IF cont = 0 THEN
          OUTPUT TO REPORT listado_3()
       END IF

       SELECT razon_social
       INTO   vrazon_siefore
       FROM   tab_siefore_local
       WHERE  codigo_siefore = vcodigo
       OUTPUT TO REPORT listado_2(vcodigo,vprecio,vrazon_siefore) #l2
       LET cont = cont + 1
    END FOREACH
    OUTPUT TO REPORT listado_4()
    FINISH REPORT listado_2
    FINISH REPORT listado_3
    FINISH REPORT listado_4

    LET cat = "cat ",G_LISTA1 CLIPPED," ",G_LISTA CLIPPED," ",G_LISTA2 CLIPPED,
              " > ", G_LISTA3
    RUN cat
    LET borra = "rm ", G_LISTA CLIPPED
    RUN borra
    LET borra = "rm ", G_LISTA1 CLIPPED
    RUN borra
    LET borra = "rm ", G_LISTA2 CLIPPED
    RUN borra
    -------------da permisos al archivo
    LET borra = "chmod 777 ", G_LISTA3 CLIPPED
    RUN borra
END FUNCTION


REPORT listado_2(vcod,vprecio,vrazon)
#l2--------------------
    DEFINE vcod     SMALLINT
    DEFINE vrazon   CHAR(8)
    DEFINE vprecio  DECIMAL(14,6)
    DEFINE cprecio  CHAR(14)
    OUTPUT
       PAGE LENGTH        1
	    LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT

    ON EVERY ROW
        PRINT 
            COLUMN 001,"03",#tipo_registro
            COLUMN 003,"01",#tipo_registro
            COLUMN 005,vcodigo_afore,
            COLUMN 008,vrazon,
            COLUMN 016,vfecha USING "YYYYMMDD",
            COLUMN 024,vprecio*1000000 USING "&&&&&&&&&&&&&&&",
            COLUMN 039,22 SPACES

END REPORT
---cza


REPORT listado_3()
#l3--------------------

    OUTPUT
       PAGE LENGTH        1
	    LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

       FORMAT
       ON EVERY ROW

        PRINT 
            COLUMN 001,"01",#tipo_registro
            COLUMN 003,"01",#afore
            COLUMN 005,vcodigo_afore,
            COLUMN 008,HOY USING "YYYYMMDD",
            COLUMN 016,45 SPACES
END REPORT


REPORT listado_4()
#l4--------------------

    OUTPUT
       PAGE LENGTH        1
	    LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

       FORMAT
       ON EVERY ROW

        PRINT 
            COLUMN 001,"09",#tipo_registro
            COLUMN 003,"01",# afore
            COLUMN 005,vcodigo_afore,
            COLUMN 008,cont USING "&&",
            COLUMN 010,51 SPACES

END REPORT
