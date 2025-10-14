########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.       					      #
#Programa          => GENERA RESULTADO DE LA CONFRONTA                        #
#Fecha             => 9 DE AGOSTO DEL 2000                                    #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
###############################################################################
DATABASE safre_af
GLOBALS
DEFINE fecha_envio DATE
DEFINE ejecuta char(200)
DEFINE v_corr  integer
DEFINE enter char(001)
DEFINE pmarca_entra  smallint
DEFINE pestado_marca smallint
DEFINE pmarca_causa  smallint
DEFINE pusuario      char(010)

        DEFINE g_cza_not RECORD
            folio             INTEGER,
            tipo_registro     CHAR(2),
            ident_servicio    CHAR(2),
            ident_operacion   CHAR(2),
            tipo_ent_origen   CHAR(2),
            cve_ent_origen    CHAR(3),
            tipo_ent_destino  CHAR(2),
            cve_ent_destino   CHAR(3),
            fecha_lote        DATE,
            consec_lote       SMALLINT,
            resulta_operacion CHAR(2),
            motivo_rechazo    CHAR(3),
            estado            SMALLINT
        END RECORD

        DEFINE reg_2 RECORD
               folio    integer  ,
	       n_seguro char(011),
	       nss      char(011)
        END RECORD

        DEFINE cont_03            integer
        DEFINE cont_02            integer

        DEFINE g_sum_not RECORD
            folio                 INTEGER,
            tipo_registro         CHAR(02),
            total_detalle_02      INTEGER,
            total_detalle_03      INTEGER
        END RECORD

        DEFINE reg_4 RECORD
            folio             INTEGER,
            tipo_registro     CHAR(2),
            ident_servicio    CHAR(2),
            ident_operacion   CHAR(2),
            tipo_ent_origen   CHAR(2),
            cve_ent_origen    CHAR(3),
            tipo_ent_destino  CHAR(2),
            cve_ent_destino   CHAR(3),
            fecha_lote        DATE,
            consec_lote       SMALLINT,
            resulta_operacion CHAR(2),
            motivo_rechazo    CHAR(3),
            estado            SMALLINT
        END RECORD

        DEFINE reg_5 RECORD
            folio                 INTEGER,
            tipo_registro         CHAR(02),
            total_detalle_02      INTEGER,
            total_detalle_03      INTEGER
        END RECORD

        DEFINE #glo #integer
            cont,
            cont1,
        --    vsolicitado,
            vlote_dia,
            tot_registros     INTEGER

        DEFINE 
            HOY                 DATE,
	    G_LISTA	       	CHAR(500),
	    cat            	CHAR(500),
	    borra          	CHAR(200),
	    aux_pausa       	CHAR(1),
	    char            	CHAR(1),
	    vcodigo_afore   	CHAR(3),
	    vfolio         	INTEGER    

	DEFINE g_paramgrales   	RECORD LIKE seg_modulo.*
        DEFINE cla_sel CHAR(100)

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


--    SELECT estado
--   INTO   vsolicitado
--    FROM   sep_status  
--    WHERE  descripcion = "SOLICITADO"

    SELECT max(a.folio) + 1
    INTO   vlote_dia
    FROM   sep_folio a
    WHERE  a.fecha_lote = HOY

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "sep"

    SELECT user 
    INTO  pusuario
    FROM  tab_afore_local

    IF (vlote_dia IS NULL OR
	vlote_dia = " ") THEN
    #IF STATUS = NOTFOUND THEN
       LET vlote_dia = 1
    END IF
   
    INSERT INTO sep_folio VALUES(vlote_dia,HOY)

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "SEPB1061" ATTRIBUTE(BORDER)
    DISPLAY "SEPC006               GENERA ARCHIVO OPERACION 28                                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                          SEPARACION DE CUENTAS                                       " AT 4,1 ATTRIBUTE(REVERSE)

    DISPLAY "    < Esc > Grabar                                        < Ctrl-C > Salir   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)



    INPUT BY NAME fecha_envio

      AFTER FIELD fecha_envio
      IF fecha_envio is NULL THEN
        ERROR "FECHA NO PUEDE SER NULA"
	SLEEP 2
	ERROR ""
	NEXT FIELD fecha_envio
      END IF
      IF fecha_envio < TODAY THEN
        ERROR "FECHA NO PUEDE MENOR AL DIA DE HOY"
	SLEEP 2
	ERROR ""
	NEXT FIELD fecha_envio
      END IF

    ON KEY (ESC)
      IF fecha_envio is NULL THEN
        ERROR "FECHA NO PUEDE SER NULA"
	SLEEP 2
	ERROR ""
	NEXT FIELD fecha_envio
      END IF
      IF fecha_envio < TODAY THEN
        ERROR "FECHA NO PUEDE MENOR AL DIA DE HOY"
	SLEEP 2
	ERROR ""
	NEXT FIELD fecha_envio
      END IF
        ERROR " PROCESANDO INFORMACION " 

        CALL genera_reporte() #gr
        CALL pregunta()

         ERROR" ARCHIVO PLANO GENERADO " 
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
    DEFINE borra CHAR(200)

    ERROR ""

    UPDATE sep_det_reg_sol_reclamante
    SET    estado = 9
    WHERE  estado in (8,10)

    LET borra = g_paramgrales.ruta_envio CLIPPED,"/",
                HOY USING"YYYYMMDD",vlote_dia USING"&&&&&",".28SEP"
    DISPLAY "Disponible en la ruta: ",borra AT 18,5 ATTRIBUTE(REVERSE)

    PROMPT " PROCESO CONCLUIDO...PRESIONE [Enter] " for CHAR aux_pausa
END FUNCTION                                                         
 
FUNCTION genera_reporte()
#gr----------------------
    SELECT * 
    INTO   g_paramgrales.*
    FROM   sep_parametro

    LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,"DETCONF"
                  CLIPPED

    LET cont = 0
           LET ejecuta = 'EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?) '
           LET ejecuta = ejecuta CLIPPED
           PREPARE qry_desmarca FROM ejecuta

    START REPORT listado_2 TO G_LISTA
    DECLARE cur_2 CURSOR FOR

SELECT c.folio ,
       c.n_seguro    ,
       c.nss         
FROM sep_det_reg_sol_reclamante c
WHERE c.estado in (8,10)
ORDER BY 1


    FOREACH cur_2 INTO reg_2.*

           SELECT a.correlativo
           INTO v_corr
           FROM cta_act_marca a
           WHERE a.nss = reg_2.n_seguro
           AND   a.marca_cod = 280

     LET pmarca_entra = 280
     LET pestado_marca = 0 
     LET pestado_marca = 0

   
--     EXECUTE qry_desmarca USING reg_2.n_seguro,
--                                pmarca_entra  ,
--                                v_corr        ,
--                                pestado_marca ,
--                                pmarca_causa  ,
--                                pusuario


        LET cont = cont + 1
        LET cont_02 = cont_02 + 1

        OUTPUT TO REPORT listado_2(reg_2.*) #l2
    END FOREACH
    FINISH REPORT listado_2

      LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED, "CZACONF" 
                    CLIPPED
    
    START REPORT listado_4 TO G_LISTA 
            LET reg_4.tipo_registro     = "01"
            LET reg_4.ident_servicio    = "02"
            LET reg_4.tipo_ent_origen   = "01"
            LET reg_4.cve_ent_origen    = vcodigo_afore
            LET reg_4.tipo_ent_destino  = "03"
            LET reg_4.cve_ent_destino   = "001"
            LET reg_4.consec_lote       = vlote_dia 
            OUTPUT TO REPORT listado_4(reg_4.*) #l4
    FINISH REPORT listado_4

      LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED, "SUMCONF" 
                    CLIPPED

    START REPORT listado_5 TO G_LISTA
            OUTPUT TO REPORT listado_5(reg_5.*) #l5
    FINISH REPORT listado_5

    LET cat = "cat ",g_paramgrales.ruta_envio CLIPPED,"/CZACONF ",
                     g_paramgrales.ruta_envio CLIPPED,"/DETCONF ",
                     g_paramgrales.ruta_envio CLIPPED,"/SUMCONF > ",
                     g_paramgrales.ruta_envio CLIPPED,"/",
                     HOY USING"YYYYMMDD",vlote_dia USING"&&&&&",".28SEP"
    RUN cat

    LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/CZACONF "
    RUN borra
    LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/DETCONF "
    RUN borra
    LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/SUMCONF "
    RUN borra

END FUNCTION

REPORT listado_2(reg_2)
#l2--------------------
DEFINE l_marca        smallint
define l_sal_part_92  dec(16,6)
define l_sal_part_97  dec(16,6)

        DEFINE reg_2 RECORD
            folio    integer,
	    n_seguro char(011),
	    nss      char(011)
        END RECORD

    DEFINE reg_x RECORD LIKE sep_det_reg_sol_reclamante.*

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW

        SELECT sum(a.monto_en_acciones)       
	     INTO   l_sal_part_92
	     FROM   dis_cuenta a 
	     WHERE  a.nss       = reg_2.n_seguro
        AND    a.subcuenta = 8

        IF l_sal_part_92 IS NULL THEN
	        LET l_sal_part_92 = 0
        END IF  

        SELECT sum(a.monto_en_acciones)       
	     INTO   l_sal_part_97
	     FROM   dis_cuenta a 
	     WHERE  a.nss       = reg_2.n_seguro
        AND    a.subcuenta = 4

        IF l_sal_part_97 IS NULL THEN
	        LET l_sal_part_97 = 0
        END IF  

        LET l_marca = 0

        SELECT "OK" 
        FROM   cta_act_marca a
        WHERE  a.nss = reg_2.n_seguro 
        AND    a.marca_cod = 280
        GROUP BY 1 

        IF STATUS <> NOTFOUND THEN
           LET l_marca = 0
        ELSE 
           let l_marca = 1
        END IF
        PRINT 
            COLUMN 001,"02"                                           ,
            COLUMN 003,cont USING"&&&&&&&&&&"                         ,
            COLUMN 013,reg_2.n_seguro                                 ,
            COLUMN 024,l_marca   USING"&"                             ,
            COLUMN 025,l_sal_part_92 * 1000000 USING "&&&&&&&&&&&&&&&",
            COLUMN 040,l_sal_part_97 * 1000000 USING "&&&&&&&&&&&&&&&",
            COLUMN 055,246 SPACES

   LET cont_03 = cont_03 + 1
   LET cont    = cont    + 1

        SELECT sum(a.monto_en_acciones)       
	     INTO   l_sal_part_92
	     FROM   dis_cuenta a 
        WHERE    a.nss        = reg_2.nss
        AND    a.subcuenta  = 8

        IF l_sal_part_92 IS NULL THEN
	   LET l_sal_part_92 = 0
        END IF  

        SELECT sum(a.monto_en_acciones)       
	     INTO   l_sal_part_97
	     FROM   dis_cuenta a 
	     WHERE   a.nss       = reg_2.nss
        AND    a.subcuenta = 4

        IF l_sal_part_97 IS NULL THEN
   	     LET l_sal_part_97 = 0
        END IF  

        LET l_marca = 0

        SELECT "OK" 
        FROM   cta_act_marca a
        WHERE  a.nss = reg_2.nss
        AND    a.marca_cod = 280
        GROUP BY 1 

        IF STATUS <> NOTFOUND THEN
           LET l_marca = 0
        ELSE 
           let l_marca = 1
        END IF

       PRINT COLUMN 001,"03"                   ,
             COLUMN 003,cont USING"&&&&&&&&&&" ,
             COLUMN 013,reg_2.nss              ,
             COLUMN 024,l_marca  USING"&"      ,
             COLUMN 025,l_sal_part_92 * 1000000 USING "&&&&&&&&&&&&&&&",
             COLUMN 040,l_sal_part_97 * 1000000 USING "&&&&&&&&&&&&&&&",
             COLUMN 055,246 SPACES

END REPORT

REPORT listado_4(reg_4)
#l4--------------------
        DEFINE reg_4 RECORD
            folio             INTEGER,
            tipo_registro     CHAR(2),
            ident_servicio    CHAR(2),
            ident_operacion   CHAR(2),
            tipo_ent_origen   CHAR(2),
            cve_ent_origen    CHAR(3),
            tipo_ent_destino  CHAR(2),
            cve_ent_destino   CHAR(3),
            fecha_lote        DATE,
            consec_lote       SMALLINT,
            resulta_operacion CHAR(2),
            motivo_rechazo    CHAR(3),
            estado            SMALLINT
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
            COLUMN 001,reg_4.tipo_registro   ,
            COLUMN 003,reg_4.ident_servicio  ,
            COLUMN 005,"28"                  ,
            COLUMN 007,reg_4.tipo_ent_origen,
            COLUMN 009,reg_4.cve_ent_origen,
            COLUMN 012,reg_4.tipo_ent_destino,
            COLUMN 014,reg_4.cve_ent_destino,
            COLUMN 017,fecha_envio USING "YYYYMMDD",
            COLUMN 025,reg_4.consec_lote USING "&&&",
            COLUMN 034,267 SPACES

END REPORT

REPORT listado_5(reg_5)
#l5--------------------

        DEFINE reg_5 RECORD
            folio                INTEGER,
            tipo_registro        CHAR(2),
            total_detalle        INTEGER,
            estado               SMALLINT
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
            COLUMN 001,"09",#tipo_registro
            COLUMN 003,cont_02  USING "&&&&&&&&&&" ,
            COLUMN 013,cont_03  USING "&&&&&&&&&&" ,
	    COLUMN 023,cont     USING "&&&&&&&&&&" ,
            COLUMN 033,268 SPACES

END REPORT
