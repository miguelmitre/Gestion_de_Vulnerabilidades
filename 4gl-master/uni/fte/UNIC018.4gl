###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => EFP 			                              #
#Programa          => REPORTE DE liquidacion de unificacion de cuentas        #
#Fecha             => 12 de marzo del 2001                                    #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha act.        => 30 agosto 2002                                          #
###############################################################################
DATABASE safre_af
GLOBALS
        DEFINE enter char(001)

        DEFINE liq_uni RECORD
            nss              CHAR(11),
            subcuenta        SMALLINT,
	    siefore          SMALLINT,
            pesos            DECIMAL(16,6),
            acciones         DECIMAL(16,6)
        END RECORD

        DEFINE liq_cta RECORD
            nss              CHAR(11),
            subcuenta        SMALLINT,
	    siefore          SMALLINT,
            pesos            DECIMAL(16,6),
            acciones         DECIMAL(16,6)
        END RECORD

        DEFINE tot_sub RECORD
            subcuenta        SMALLINT,
	    siefore          SMALLINT,
            pesos            DECIMAL(16,6),
            acciones         DECIMAL(16,6)
        END RECORD

        DEFINE total RECORD
            pesos            DECIMAL(16,6),
            acciones         DECIMAL(16,6)
        END RECORD

        DEFINE #glo #integer
            cont              ,
            cont1             ,
            i                 ,
            vfolio2           , 
            vfolio            , 
            tot_registros     INTEGER

        DEFINE #glo #decimal
            total_pesos       ,
            total_acciones    DECIMAL(16,6)

        DEFINE #glo #date
            HOY               DATE

        DEFINE #glo #char
	    G_LISTA	      CHAR(500),
	    G_LISTA1	      CHAR(500),
	    G_LISTA2	      CHAR(100),
	    G_LISTA3	      CHAR(100),
	    imprime           CHAR(100),
	    borra             CHAR(100),
	    cat               CHAR(500),
	    aux_pausa         CHAR(001),
	    char              CHAR(001),
            bold              CHAR(032),
            raya              CHAR(120),
            cla_sel           CHAR(250),
            nss_unifica       CHAR(011),
            cve_ent_uni       CHAR(003),
            cve_ent_cta       CHAR(003),
            vcodigo_afore     CHAR(003),
            vrazon_social     CHAR(050)

    DEFINE g_paramgrales   	RECORD LIKE seg_modulo.*

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("UNIC018.log")
    CALL inicio()  #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT codigo_afore,
           razon_social
    INTO   vcodigo_afore,
           vrazon_social
    FROM   tab_afore_local

    LET bold  = '\033e\033(s218T\033(s12H\033(s7B'
    LET raya = '_____________________________________________',
               '_____________________________________________',
               '____________________'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0171" ATTRIBUTE(BORDER)
    DISPLAY "UNIC018         REPORTE DE LIQUIDACION DE UNIFICACION                           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                            DE CUENTAS                                          " AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY "          [ Esc ] Iniciar                              [ Ctrl-C ] Salir         " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio

      AFTER FIELD vfolio
		
       SELECT "X"
       FROM   uni_unificador
       WHERE  folio_liquida = vfolio
       GROUP BY 1

       IF STATUS = NOTFOUND THEN
          PROMPT " NO HAY REGISTROS DE LIQUIDACION CON ESE NUMERO DE FOLIO "
          FOR CHAR enter
	  EXIT PROGRAM
       END IF

    ON KEY (ESC)
    ERROR " PROCESANDO INFORMACION " 

      #CALL genera_reporte() #gr
      CALL genera_archivo() #gr

      ERROR " "
      DISPLAY "ARCHIVO GENERADO EN : ",g_paramgrales.ruta_rescate CLIPPED,
            "/",vfolio,".LIQ22" AT 16,1
      PROMPT "PROCESO FINALIZADO ...[ENTER] PARA CONTINUAR "
      FOR enter
      EXIT INPUT
      ERROR " REPORTE GENERADO " 
      SLEEP 2
      EXIT PROGRAM
      ERROR ""

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM

    END INPUT

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION
FUNCTION pregunta()
    ERROR ""

    PROMPT " DESEA GENERAR EL ARCHIVO  S/N  ?  " for CHAR aux_pausa
END FUNCTION                                                         

FUNCTION genera_reporte()
#gr----------------------

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "uni"

    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "22liq" CLIPPED

    INITIALIZE liq_uni  TO NULL 

    START REPORT listado_2 TO G_LISTA
    DECLARE cur_1 CURSOR FOR

        SELECT nss,
	       subcuenta,
	       siefore,
	       monto_en_pesos,
	       monto_en_acciones
        FROM   dis_cuenta
        WHERE  folio           = vfolio
        AND    subcuenta      <> 0
        AND    tipo_movimiento in(1,4)
        ORDER BY 1,2

    FOREACH cur_1 INTO liq_uni.*

        OUTPUT TO REPORT listado_2(liq_uni.*) #l2

    END FOREACH

    FINISH REPORT listado_2

    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "22liq1" CLIPPED

    INITIALIZE liq_uni  TO NULL 

    START REPORT listado_3 TO G_LISTA
    DECLARE cur_2 CURSOR FOR

        SELECT nss,
	       subcuenta,
	       siefore,
	       monto_en_pesos,
	       monto_en_acciones
        FROM   dis_cuenta
        WHERE  folio           = vfolio
        AND    subcuenta       <> 0
        AND    tipo_movimiento in(241,242,243,244)
        ORDER BY 1,2

    FOREACH cur_2 INTO liq_cta.*

        OUTPUT TO REPORT listado_3(liq_cta.*) #l3

    END FOREACH

    FINISH REPORT listado_3

    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "22liq2" CLIPPED

    START REPORT listado_4 TO G_LISTA
    DECLARE cur_3 CURSOR FOR

        SELECT subcuenta,
	       siefore,
	       SUM(monto_en_pesos),
	       SUM(monto_en_acciones)
        FROM   dis_cuenta
        WHERE  folio           = vfolio
        AND    subcuenta      <> 0
        AND    tipo_movimiento in(1,4)
        GROUP BY 1,2
        ORDER BY 1

    FOREACH cur_3 INTO tot_sub.*

        OUTPUT TO REPORT listado_4(tot_sub.*) #l4

    END FOREACH

    FINISH REPORT listado_4

    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "22liq3" CLIPPED

    START REPORT listado_5 TO G_LISTA
        SELECT SUM(monto_en_pesos),
	       SUM(monto_en_acciones)
        INTO   total_pesos,
	       total_acciones
        FROM   dis_cuenta
        WHERE  folio           = vfolio
        AND    subcuenta      <> 0
        AND    tipo_movimiento in(1,4)

    OUTPUT TO REPORT listado_5(total_pesos,total_acciones) #l5

    FINISH REPORT listado_5

    LET G_LISTA2 = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,"LIQ22"
               
    LET G_LISTA3 = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,"22*"

    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      ";sed -e '/^$/d' ","22liq" CLIPPED,
      " >", "22LIQ" CLIPPED

    RUN G_LISTA
    
    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      ";sed -e '/^$/d' ","22liq1" CLIPPED,
      " >", "22LIQ1" CLIPPED

    RUN G_LISTA

    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      ";sed -e '/^$/d' ","22liq2" CLIPPED,
      " >", "22LIQ2" CLIPPED

    RUN G_LISTA

    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      ";sed -e '/^$/d' ","22liq3" CLIPPED,
      " >", "22LIQ3" CLIPPED

    RUN G_LISTA

    LET cat = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
              ";cat   22LIQ 22LIQ1 22LIQ2 22LIQ3 > LIQ22"
    RUN cat
    LET imprime = "vi ",G_LISTA2
    --LET imprime = "lp ",G_LISTA2
    RUN imprime
    LET borra   = "rm ",G_LISTA2
    RUN borra
    LET borra   = "rm ",G_LISTA3
    RUN borra

END FUNCTION
FUNCTION genera_archivo()
#gr----------------------

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "uni"

    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "liq1" CLIPPED

    INITIALIZE liq_uni  TO NULL 

    START REPORT listado_6 TO G_LISTA
    DECLARE cur_4 CURSOR FOR

        SELECT nss,
	       subcuenta,
	       siefore,
	       monto_en_pesos,
	       monto_en_acciones
        FROM   dis_cuenta
        WHERE  folio           = vfolio
        AND    subcuenta      <> 0
        AND    tipo_movimiento in(1,4)
        ORDER BY 1,2

    FOREACH cur_4 INTO liq_uni.*

        OUTPUT TO REPORT listado_6(liq_uni.*) #l2

    END FOREACH

    FINISH REPORT listado_6

    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "liq2" CLIPPED

    INITIALIZE liq_uni  TO NULL 

    START REPORT listado_7 TO G_LISTA
    DECLARE cur_5 CURSOR FOR

        SELECT nss,
	       subcuenta,
	       siefore,
	       monto_en_pesos,
	       monto_en_acciones
        FROM   dis_cuenta
        WHERE  folio           = vfolio
        AND    subcuenta       <> 0
        AND    tipo_movimiento in(241,242,243,244)
        ORDER BY 1,2

    FOREACH cur_5 INTO liq_cta.*

        OUTPUT TO REPORT listado_7(liq_cta.*) #l7

    END FOREACH

    FINISH REPORT listado_7

    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "liq3" CLIPPED

    START REPORT listado_8 TO G_LISTA
    DECLARE cur_6 CURSOR FOR

        SELECT subcuenta,
	       siefore,
	       SUM(monto_en_pesos),
	       SUM(monto_en_acciones)
        FROM   dis_cuenta
        WHERE  folio           = vfolio
        AND    subcuenta      <> 0
        AND    tipo_movimiento in(1,4)
        GROUP BY 1,2
        ORDER BY 1

    FOREACH cur_6 INTO tot_sub.*

        OUTPUT TO REPORT listado_8(tot_sub.*) #l4

    END FOREACH

    FINISH REPORT listado_8

    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "liq4" CLIPPED

    START REPORT listado_9 TO G_LISTA
        SELECT SUM(monto_en_pesos),
	       SUM(monto_en_acciones)
        INTO   total_pesos,
	       total_acciones
        FROM   dis_cuenta
        WHERE  folio           = vfolio
        AND    subcuenta      <> 0
        AND    tipo_movimiento in(1,4)

    OUTPUT TO REPORT listado_9(total_pesos,total_acciones) #l5

    FINISH REPORT listado_9

    LET G_LISTA3 = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,"LIQ*"
    LET G_LISTA2 = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,"liq*"
    #LET G_LISTA2 = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
		   #vfolio,".LIQ22"

    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      ";sed -e '/^$/d' ","liq1" CLIPPED,
      " >", "LIQ1" CLIPPED

    RUN G_LISTA
    
    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      ";sed -e '/^$/d' ","liq2" CLIPPED,
      " >", "LIQ2" CLIPPED

    RUN G_LISTA

    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      ";sed -e '/^$/d' ","liq3" CLIPPED,
      " >", "LIQ3" CLIPPED

    RUN G_LISTA

    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      ";sed -e '/^$/d' ","liq4" CLIPPED,
      " >", "LIQ4" CLIPPED

    RUN G_LISTA

    LET cat = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
              ";cat   LIQ1 LIQ2 LIQ3 LIQ4 > ",vfolio,".LIQ22" CLIPPED
    RUN cat
    LET imprime = "vi ",G_LISTA2
    --LET imprime = "lp ",G_LISTA2
    RUN imprime
    LET borra   = "rm ",G_LISTA2
    RUN borra
    LET borra   = "rm ",G_LISTA3
    RUN borra

END FUNCTION
REPORT listado_2(reg_2)
#l2--------------------

    DEFINE reg_2 RECORD
        nss              CHAR(11),
	subcuenta        SMALLINT,
	siefore          SMALLINT,
	pesos            DECIMAL(16,6),
	acciones         DECIMAL(16,6)
    END RECORD

    OUTPUT
        PAGE LENGTH 60
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
       PRINT bold

       PRINT 
            COLUMN 004, vrazon_social,
            COLUMN 080, HOY USING "DD-MM-YYYY"
            
       PRINT 
            COLUMN 004,"UNIC018",
            COLUMN 029,"REPORTE DE UNIFICACION DE CUENTAS",
            COLUMN 080, "P\240gina  ",pageno USING "##"

       PRINT 
            COLUMN 004,"Folio :  ",vfolio USING "######"

       PRINT #peque,
            COLUMN 003,raya CLIPPED

       PRINT  
            COLUMN 005,"CUENTAS UNIFICADORAS "
       PRINT  
            COLUMN 012,"NSS ",
            COLUMN 023,"Subcuenta ",
	    COLUMN 035,"Siefore",
            COLUMN 052,"Pesos",
            COLUMN 078,"Acciones"

       PRINT 
            COLUMN 003,raya CLIPPED

    ON EVERY ROW
        PRINT 
            COLUMN 008,reg_2.nss,
            COLUMN 023,reg_2.subcuenta,
            COLUMN 037,reg_2.siefore USING "<<",
            COLUMN 041,reg_2.pesos,
            COLUMN 068,reg_2.acciones
END REPORT
REPORT listado_3(reg_3)
#l3--------------------

    DEFINE reg_3 RECORD
        nss              CHAR(11),
	subcuenta        SMALLINT,
	siefore          SMALLINT,
	pesos            DECIMAL(16,6),
	acciones         DECIMAL(16,6)
    END RECORD

    OUTPUT
        PAGE LENGTH 60
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
       PRINT bold

       PRINT #peque,
            COLUMN 003,raya CLIPPED

       PRINT  
            COLUMN 005,"CUENTAS UNIFICADAS "
       PRINT  
            COLUMN 012,"NSS ",
            COLUMN 023,"Subcuenta ",
	    COLUMN 035,"Siefore",
            COLUMN 052,"Pesos",
            COLUMN 078,"Acciones"

       PRINT 
            COLUMN 003,raya CLIPPED

    ON EVERY ROW
        PRINT 
            COLUMN 008,reg_3.nss,
            COLUMN 023,reg_3.subcuenta,
            COLUMN 037,reg_3.siefore USING "<<",
            COLUMN 041,reg_3.pesos,
            COLUMN 068,reg_3.acciones
END REPORT

REPORT listado_4(reg_4)
#l4--------------
        DEFINE reg_4 RECORD
	   subcuenta        SMALLINT,
	   siefore          SMALLINT,
	   pesos            DECIMAL(16,6),
	   acciones         DECIMAL(16,6)
        END RECORD

    OUTPUT
        PAGE LENGTH 60
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
       PRINT bold

       PRINT #peque,
            COLUMN 003,raya CLIPPED
       PRINT 
            COLUMN 005,"TOTALES POR SUBCUENTA"

       PRINT  
            COLUMN 023,"Subcuenta ",
	    COLUMN 035,"Siefore",
            COLUMN 052,"Pesos",
            COLUMN 078,"Acciones"

       PRINT 
            COLUMN 003,raya CLIPPED

    ON EVERY ROW
        PRINT 

        PRINT 
            COLUMN 023,reg_4.subcuenta,
            COLUMN 037,reg_4.siefore USING "<<",
            COLUMN 041,reg_4.pesos,
            COLUMN 068,reg_4.acciones

        PRINT 

        PRINT 

END REPORT
REPORT listado_5(reg_5)
#l5--------------------

    DEFINE reg_5 RECORD
	pesos            DECIMAL(16,6),
	acciones         DECIMAL(16,6)
    END RECORD

    OUTPUT
        PAGE LENGTH 3
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW

        PRINT 
        PRINT 
            COLUMN 003,raya CLIPPED
        PRINT 
            COLUMN 005,"TOTAL DE OPERACION",
            COLUMN 041,reg_5.pesos,
            COLUMN 068,reg_5.acciones

END REPORT

REPORT listado_6(reg_2)
#l6--------------------
    DEFINE reg_2 RECORD
        nss              CHAR(11),
	subcuenta        SMALLINT,
	siefore          SMALLINT,
	pesos            DECIMAL(16,6),
	acciones         DECIMAL(16,6)
    END RECORD

    OUTPUT
        PAGE LENGTH 60
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"02",
            COLUMN 003,reg_2.nss,
            COLUMN 014,reg_2.subcuenta USING"&&",
            COLUMN 016,reg_2.siefore USING"<<",
            COLUMN 018,reg_2.pesos     USING"#########&.&&&&&&",
            COLUMN 035,reg_2.acciones  USING"#########&.&&&&&&"
END REPORT
REPORT listado_7(reg_3)
#l7--------------------

    DEFINE reg_3 RECORD
        nss              CHAR(11),
	subcuenta        SMALLINT,
	siefore          SMALLINT,
	pesos            DECIMAL(16,6),
	acciones         DECIMAL(16,6)
    END RECORD

    OUTPUT
        PAGE LENGTH 60
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"03",
            COLUMN 003,reg_3.nss,
            COLUMN 014,reg_3.subcuenta USING"&&",
            COLUMN 016,reg_3.siefore USING"<<",
            COLUMN 018,reg_3.pesos     USING"#########&.&&&&&&",
            COLUMN 035,reg_3.acciones  USING"#########&.&&&&&&"
END REPORT

REPORT listado_8(reg_4)
#l8--------------------
        DEFINE reg_4 RECORD
	   subcuenta        SMALLINT,
	   siefore          SMALLINT,
	   pesos            DECIMAL(16,6),
	   acciones         DECIMAL(16,6)
        END RECORD

    OUTPUT
        PAGE LENGTH 60
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"91",
            COLUMN 003,reg_4.subcuenta USING"&&",
            COLUMN 005,reg_4.siefore USING"<<",
            COLUMN 007,reg_4.pesos     USING"#########&.&&&&&&",
            COLUMN 024,reg_4.acciones  USING"#########&.&&&&&&"

END REPORT
REPORT listado_9(reg_5)
#l9--------------------

    DEFINE reg_5 RECORD
	pesos            DECIMAL(16,6),
	acciones         DECIMAL(16,6)
    END RECORD

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW

        PRINT 
            COLUMN 001,"99",
            COLUMN 003,vfolio          USING"#########&",
            COLUMN 013,reg_5.pesos     USING"#########&.&&&&&&",
            COLUMN 030,reg_5.acciones  USING"#########&.&&&&&&"

END REPORT

