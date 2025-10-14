###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.       					      #
#Programa          => GENERA RESULTADO DE LA CONFRONTA op 27        
#Fecha             => 5 DE JULIO  DEL 2010
#Actualizado       => JESUS YAÑEZ MORENO
###############################################################################
DATABASE safre_af
GLOBALS
DEFINE g_enter char(001)
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
            folio                 INTEGER,
            tipo_registro         CHAR(2),
            cont_servicio         INTEGER,
            nss                   CHAR(11),
            rfc                   CHAR(13),
            curp                  CHAR(18),
            tipo_ent_admon        CHAR(2),
            clave_admon           CHAR(3),
            paterno               CHAR(40),
            materno               CHAR(40),
            nombre                CHAR(40),
            fecha_nacimiento      DATE,
            ent_nacimiento        CHAR(2),
            sexo                  CHAR(1),
            nombre_procanase      CHAR(40),
            fecha_afiliacion      DATE,
            fecha_marca           DATE,
            diag_confronta        CHAR(2),
            clasifica_separacion  CHAR(1),
            credito               char(1),
            resulta_operacion     CHAR(2),
            diag_proceso1         CHAR(3),
            diag_proceso2         CHAR(3),
            diag_proceso3         CHAR(3),
            traspaso_previo       char(02),
            idSolicitudSeparacion integer
        END RECORD
        DEFINE cont_02            ,
               cont_03            integer
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
            vconfronta,
            vsolicitado,
            vlote_dia,
            tot_registros     INTEGER

        DEFINE 
            HOY                 DATE,
	    G_LISTA	       	CHAR(500),
	    cat            	CHAR(500),
	    borra          	CHAR(200),
	    aux_pausa       	CHAR(1),
	    char            	CHAR(1),
	    vcodigo_afore   	CHAR(3)

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

    OPEN WINDOW ventana_1  AT 2,2 
WITH 21 rows, 78 columns attribute (border)
    DISPLAY "SEPC006            GENERA RESPUESTA DE CONFRONTA  OP 27                              " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "    < Esc > Grabar                                        < Ctrl-C > Salir   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)



    WHILE TRUE
       PROMPT "   DESEA GENERAR EL ARCHIVO DE RESPUESTA op.27 [S/N] ? " FOR g_enter
                IF    g_enter  MATCHES "[sSnN]" THEN
                      IF   g_enter  MATCHES "[sS]" THEN
                           EXIT WHILE
                      ELSE
                           ERROR"PROCESO CANCELADO ..."
                           SLEEP 2
                           EXIT PROGRAM
                      END IF
                END IF
    END WHILE

      CALL genera_reporte() #gr
      CALL pregunta()

      ERROR" ARCHIVO PLANO GENERADO " 
      SLEEP 2
      EXIT PROGRAM
      ERROR ""


    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION pregunta()
    DEFINE borra CHAR(100)

    ERROR ""

    LET borra = g_paramgrales.ruta_envio CLIPPED,"/",
                HOY USING"YYYYMMDD",vlote_dia USING"&&&&&",
                ".27SEP"

    DISPLAY "TOTAL REGISTROS PROCESADOS: ",cont AT 10,5
    DISPLAY "TOTAL REGISTROS DETALLE 02: ",cont_02 AT 12,5
    DISPLAY "TOTAL REGISTROS DETALLE 03: ",cont_03 AT 14,5

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
    LET cont_02 = 0
    LET cont_03 = 0

    START REPORT listado_2 TO G_LISTA
    DECLARE cur_2 CURSOR FOR
        SELECT a.*
        FROM   sep_det_solicitud  a ,
               sep_det_reg_sol_reclamante b
        WHERE  b.correlativo = a.idSolicitudSeparacion
        AND    b.estado = '3'
        ORDER BY a.n_seguro

    FOREACH cur_2 INTO reg_2.*

        IF reg_2.diag_confronta = '02' THEN

           SELECT a.correlativo
           INTO v_corr
           FROM cta_act_marca a
           WHERE a.nss = reg_2.nss
           AND   a.marca_cod = 280

           LET ejecuta =
           'EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?) '
           LET ejecuta = ejecuta CLIPPED
           PREPARE qry_desmarca FROM ejecuta

           LET pmarca_entra  = 280
           LET pestado_marca = 30
           LET pmarca_causa  = 280

           EXECUTE qry_desmarca USING reg_2.nss     ,
                                      pmarca_entra  ,
                                      v_corr        ,
                                      pestado_marca ,
                                      pmarca_causa  ,
                                      pusuario     

        END IF
        LET cont = cont + 1
        LET cont_02 = cont_02 + 1

        OUTPUT TO REPORT listado_2(reg_2.*) #l2

        update sep_det_reg_sol_reclamante 
        set    estado = 4 --enviado
        where  correlativo = reg_2.idSolicitudSeparacion

    END FOREACH
    FINISH REPORT listado_2

      LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED, "CZACONF" 
                    CLIPPED
    
    START REPORT listado_4 TO G_LISTA 

            LET reg_4.tipo_registro    = "01"
            LET reg_4.ident_servicio   = "02"
            LET reg_4.ident_operacion  = "27"
            LET reg_4.tipo_ent_destino = "01"

            SELECT a.codigo_afore 
            INTO reg_4.cve_ent_destino
            FROM tab_afore_local a

            LET reg_4.tipo_ent_origen  = "03"
            LET reg_4.cve_ent_origen   = "001"
            LET reg_4.consec_lote      = vlote_dia 

            OUTPUT TO REPORT listado_4(reg_4.*) #l4
    FINISH REPORT listado_4

      LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED, "SUMCONF" 
                    CLIPPED

    START REPORT listado_5 TO G_LISTA
            OUTPUT TO REPORT listado_5() #l5
    FINISH REPORT listado_5

    LET cat = "cat ",g_paramgrales.ruta_envio CLIPPED,"/CZACONF ",
                     g_paramgrales.ruta_envio CLIPPED,"/DETCONF ",
                     g_paramgrales.ruta_envio CLIPPED,"/SUMCONF > ",
                     g_paramgrales.ruta_envio CLIPPED,"/",
                     HOY USING"YYYYMMDD",vlote_dia USING"&&&&&",
                     ".27SEP"
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

        DEFINE reg_2 RECORD
            folio                 INTEGER,
            tipo_registro         CHAR(2),
            cont_servicio         INTEGER,
            nss                   CHAR(11),
            rfc                   CHAR(13),
            curp                  CHAR(18),
            tipo_ent_admon        CHAR(2),
            clave_admon           CHAR(3),
            paterno               CHAR(40),
            materno               CHAR(40),
            nombre                CHAR(40),
            fecha_nacimiento      DATE,
            ent_nacimiento        CHAR(2),
            sexo                  CHAR(1),
            nombre_procanase      CHAR(40),
            fecha_afiliacion      DATE,
            fecha_marca           DATE,
            diag_confronta        CHAR(2),
            clasifica_separacion  CHAR(1),
            credito               char(1),
            resulta_operacion     CHAR(2),
            diag_proceso1         CHAR(3),
            diag_proceso2         CHAR(3),
            diag_proceso3         CHAR(3),
            traspaso_previo       char(02),
            idSolicitudSeparacion integer
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

      IF reg_2.credito <> 1 THEN 
	 LET reg_2.credito = " "
      END IF

        PRINT 
            COLUMN 001,"02"                                       ,
            COLUMN 003,cont USING"&&&&&&&&&&"                     ,
            COLUMN 013,reg_2.nss                                  ,
            COLUMN 024,reg_2.rfc                                  ,
            COLUMN 037,reg_2.curp                                 ,
            COLUMN 055,"01"                                 ,
            COLUMN 057,reg_2.clave_admon                          ,
            COLUMN 060,reg_2.paterno                              ,
            COLUMN 100,reg_2.materno                              ,
            COLUMN 140,reg_2.nombre                               ,
            COLUMN 180,reg_2.fecha_nacimiento USING "yyyymmdd"    ,
            COLUMN 188,reg_2.ent_nacimiento                       ,
            COLUMN 190,reg_2.sexo                                 ,
            COLUMN 191,reg_2.nombre_procanase                     ,
            COLUMN 241,reg_2.fecha_afiliacion USING "yyyymmdd"    ,  
            COLUMN 249,reg_2.fecha_marca USING "yyyymmdd"         ,
            COLUMN 257,reg_2.diag_confronta                       ,
            COLUMN 259,reg_2.clasifica_separacion                 ,
            COLUMN 260,reg_2.credito                              ,
{
            COLUMN 261,reg_2.resulta_operacion,
            COLUMN 270,reg_2.diag_proceso1,
            COLUMN 273,reg_2.diag_proceso2,
            COLUMN 276,reg_2.diag_proceso3,
            COLUMN 279,21 SPACES
}
            COLUMN 260,40 SPACES

   LET reg_x.nss = NULL

   SELECT a.* 
   INTO   reg_x.*
   FROM   sep_det_reg_sol_reclamante a
   WHERE  a.correlativo = reg_2.idSolicitudSeparacion

   LET cont    = cont + 1
   LET cont_03 = cont_03 + 1

       PRINT COLUMN 001,"03"                    ,
             COLUMN 003,cont USING"&&&&&&&&&&"  ,
             COLUMN 013,reg_x.nss               ,
             COLUMN 024,277 SPACES

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
            COLUMN 001,reg_4.tipo_registro    ,#tipo_registro
            COLUMN 003,reg_4.ident_servicio   ,
            COLUMN 005,reg_4.ident_operacion  ,
            COLUMN 007,reg_4.tipo_ent_destino ,
            COLUMN 009,reg_4.cve_ent_destino  ,
            COLUMN 012,reg_4.tipo_ent_origen  ,
            COLUMN 014,reg_4.cve_ent_origen   ,
            COLUMN 017,HOY USING "YYYYMMDD"   ,
            COLUMN 025,reg_4.consec_lote USING "&&&",
            COLUMN 028,273 SPACES

END REPORT

REPORT listado_5()
#l5--------------------

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
            COLUMN 003,cont_02  USING "&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,cont_03  USING "&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 023,cont     USING "&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 023,268 SPACES

END REPORT
