###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                      			      #
#Programa          => GENERA RESULTADO DE LA CONFRONTA                        #
#Fecha             => 8 de marzo del 2000                                     #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha Act         =>  6 de marzo 2003                                        #
#Fecha Act         =>  20 DE ABRIL DE 2004                                    #
#Modificado por    => OMAR SANDOVAL BADILLO                                   #
#Fecha modificacion=> 12 noviembre 2004                                       #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE enter    CHAR(001)
    DEFINE reg_2    RECORD LIKE uni_unificador.*
    DEFINE reg_3    RECORD LIKE uni_unificado.*

    DEFINE reg_4    RECORD LIKE uni_cza_notifica.*
    DEFINE reg_5    RECORD LIKE uni_sum_notifica.*

    DEFINE reg_6    RECORD LIKE uni_unificador.*
    DEFINE reg_7    RECORD LIKE uni_unificado.*

    DEFINE #glo #integer
        cont              ,
        cont1             ,
        cont2             ,
        cont3             ,
        cont4             ,
        cont5             ,
        tot_registros     INTEGER

    DEFINE 
        HOY               DATE,
        G_LISTA	          CHAR(500),
        cat               CHAR(500),
        borra             CHAR(200),
        aux_pausa         CHAR(1),
        char              CHAR(1),
        vcodigo_afore     CHAR(3),
        vaceptado         INTEGER,
        vconfronta        INTEGER,
        vintrafore        INTEGER,
        vimprocede        INTEGER,
        vcedente          INTEGER,
        vlote             INTEGER, 
        vfolio2           INTEGER, 
        vfolio            INTEGER    

    DEFINE g_paramgrales  RECORD LIKE seg_modulo.*

    DEFINE cla_sel        CHAR(100),
           cla_sel1       CHAR(100),
           ejecuta        CHAR(100) 

    DEFINE disp1,
           disp2,
           disp3          SMALLINT
          
END GLOBALS
##############################################
MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("UNIC006.log")

    CALL inicio() 
    CALL proceso_principal()

END MAIN
##############################################
FUNCTION inicio()

    LET HOY = TODAY

    SELECT codigo_afore
    INTO   vcodigo_afore
    FROM   tab_afore_local

    SELECT estado
    INTO   vconfronta
    FROM   uni_status
    WHERE  descripcion = "CONFRONTADO"

    SELECT estado
    INTO   vaceptado
    FROM   uni_status
    WHERE  descripcion = "ACEPTADO CONFRONTA"

    SELECT estado
    INTO   vintrafore
    FROM   uni_status
    WHERE  descripcion = "INTRA AFORE"

    SELECT estado
    INTO   vcedente
    FROM   uni_status
    WHERE  descripcion = "TRASPASO CEDENTE"

    SELECT estado
    INTO   vimprocede
    FROM   uni_status
    WHERE  descripcion = "IMPROCEDENTE"

    SELECT MAX(lotes_num) + 1
    INTO   vlote
    FROM   tab_lote
    WHERE  lotes_cod = 13
    AND    lotes_fecha = HOY

    IF vlote IS NULL THEN
        LET vlote = 1
    END IF

    INSERT INTO tab_lote VALUES(HOY,13,"UNIFICACION DE CUENTAS",0,vlote)

END FUNCTION
##############################################
FUNCTION proceso_principal()

    DEFINE disp1,
           disp2,
           disp3          SMALLINT

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0061" ATTRIBUTE(BORDER)
    DISPLAY "UNIC006            GENERA RESPUESTA DE CONFRONTA PARA                                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                         UNIFICACION DE CUENTAS                                      " AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY "    < Esc > Grabar                                        < Ctrl-C > Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio

      AFTER FIELD vfolio

          SELECT "X"
          FROM   uni_unificador
          WHERE  folio = vfolio
          AND    estado = vconfronta
          GROUP BY 1

      #IF vfolio IS NULL THEN
      IF STATUS = NOTFOUND THEN
           ERROR "NO EXISTEN SOLICITUDES CON ESTE FOLIO"
	   SLEEP 3
	   EXIT PROGRAM
      END IF

      ON KEY (ESC)
          DISPLAY " PROCESANDO INFORMACION " 

      CALL genera_reporte()   #gr
#-------------- actualizacion de estados
      CALL intra()            #ain
      CALL improcedente()     #aim
      CALL cedente()          #ac

      CALL actualiza_registrado()

      #CALL pregunta()

      ERROR " "
      DISPLAY "ARCHIVO GENERADO EN : ",g_paramgrales.ruta_envio CLIPPED,
	      "/",HOY USING"YYYYMMDD",".21UN",vfolio using "&&&" AT 10,10
      PROMPT "PROCESO FINALIZADO ...[ENTER] PARA CONTINUAR "
      FOR enter

      DISPLAY "                       DESMARCA FAMILIAS NO PROCEDENTES                        " AT 12,1 ATTRIBUTE(REVERSE)

      PROMPT "PROCESO DE DESMARCAJE ...[ENTER] PARA CONTINUAR "
      FOR enter

      CALL UNIC012(vfolio)
         RETURNING disp1,
                   disp2,
                   disp3

      DISPLAY "Total a desmarcar : ",disp1 AT 14,10
      DISPLAY "Unificadores      : ",disp2 AT 16,10
      DISPLAY "Unificados        : ",disp3 AT 18,10

      PROMPT "GENERACION CARTA (30250) ...[ENTER] PARA CONTINUAR "
      FOR enter

      SELECT *
      INTO   g_paramgrales.*
      FROM   seg_modulo
      WHERE  modulo_cod = "int"

      LET ejecuta = "cd ",g_paramgrales.ruta_exp CLIPPED,";",
		    "fglgo INTB0144.4gi " ,"S " ,vfolio  
      RUN ejecuta

      EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM

    END INPUT

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION
##############################################
{
FUNCTION pregunta()
    DEFINE borra CHAR(40)

    ERROR ""

    LET borra = g_paramgrales.ruta_envio CLIPPED,"/",
                HOY USING"YYYYMMDD",".21UN"

    DISPLAY "Archivo disponible en la ruta: ",borra AT 17,5 ATTRIBUTE(REVERSE)

    PROMPT " PROCESO CONCLUIDO...PRESIONE [Enter] " for CHAR aux_pausa

END FUNCTION                                                         
}
##############################################
FUNCTION genera_reporte()

    DEFINE   nss_unifi CHAR(11)

--    DISPLAY " PROCESANDO INFORMACION " 

    SELECT * 
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "uni"

    LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,"DETCONF"
                  CLIPPED

    LET cont  = 0
    LET cont1 = 0
    LET cont4 = 0
    LET cont5 = 0

    LET cla_sel = "SELECT * ",
                  "FROM   uni_unificado ",
                  "WHERE  folio = ? ",
                  "AND    nss_uni = ? ",
                  "AND    estado = 20 "

    PREPARE claexe FROM cla_sel
    DECLARE cur_3 CURSOR FOR claexe

    START REPORT listado_2 TO G_LISTA
    DECLARE cur_2 CURSOR FOR
        SELECT *
        FROM   uni_unificador
        WHERE  folio          = vfolio
        AND    cve_ent_nss    = vcodigo_afore
        AND    status_convoca in(1,2)
        AND    estado         = vconfronta
        ORDER BY nss_uni

    FOREACH cur_2 INTO reg_2.*

        IF reg_2.cve_afo_recep = "000" THEN
            LET reg_2.cve_afo_recep = ""
        END IF

        LET cont = cont + 1
        OUTPUT TO REPORT listado_2(reg_2.*) #l2
    END FOREACH
    FINISH REPORT listado_2

    LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,"DETCONF1"
                  CLIPPED

    LET cont2 = 0
    LET cont3 = 0

    LET cla_sel1 = "SELECT * ",
                   "FROM   uni_unificado ",
                   "WHERE  folio = ? ",
                   "AND    nss_uni = ? ",
                   "AND    estado = 20 "

    PREPARE claexe1 FROM cla_sel1
    DECLARE cur_8 CURSOR FOR claexe1

    START REPORT listado_6 TO G_LISTA
    DECLARE cur_6 CURSOR FOR
        SELECT nss_uni
        FROM   uni_unificado
        WHERE  folio          = vfolio
        AND    cve_ent_cta1   = vcodigo_afore
        AND    status_convoca in(1,2)
        AND    estado         = vconfronta
        GROUP BY 1
        ORDER BY 1

    FOREACH cur_6 INTO nss_unifi

       DECLARE cur_7 CURSOR FOR
           SELECT *
           FROM   uni_unificador
           WHERE  nss_uni  = nss_unifi
           AND    folio    = vfolio
           #AND    estado  = vconfronta
           ORDER BY nss_uni

       FOREACH cur_7 INTO reg_6.*
           IF reg_6.cve_afo_recep = "000" THEN
              LET reg_6.cve_afo_recep = ""
           END IF

           LET cont = cont + 1
           OUTPUT TO REPORT listado_6(reg_6.*) #l2
       END FOREACH
    END FOREACH

    FINISH REPORT listado_6

    LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED, "CZACONF" 
                  CLIPPED
    
    DECLARE cur_4 CURSOR FOR
    SELECT *
    FROM   uni_cza_notifica
    WHERE  folio  = vfolio
    #AND    estado = vconfronta


    START REPORT listado_4 TO G_LISTA 
        FOREACH cur_4 INTO reg_4.*
            LET reg_4.tipo_ent_origen  = "01"
            LET reg_4.cve_ent_origen   = vcodigo_afore
            LET reg_4.tipo_ent_destino = "03"
            LET reg_4.cve_ent_destino  = "001"
            LET reg_4.fecha_presenta   = HOY 
            LET reg_4.consec_lote      = vlote 
            OUTPUT TO REPORT listado_4(reg_4.*) #l4
        END FOREACH
    FINISH REPORT listado_4

    LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED, "SUMCONF" 
                  CLIPPED

    DECLARE cur_5 CURSOR FOR
    SELECT *
    FROM   uni_sum_notifica
    WHERE  folio  = vfolio
    #AND    estado = vconfronta

    START REPORT listado_5 TO G_LISTA
        FOREACH cur_5 INTO reg_5.*
            OUTPUT TO REPORT listado_5(reg_5.*) #l5
        END FOREACH
    FINISH REPORT listado_5

    LET cat = "cat ",g_paramgrales.ruta_envio CLIPPED,"/CZACONF ",
                     g_paramgrales.ruta_envio CLIPPED,"/DETCONF ",
                     g_paramgrales.ruta_envio CLIPPED,"/DETCONF1 ",
                     g_paramgrales.ruta_envio CLIPPED,"/SUMCONF > ",
                     g_paramgrales.ruta_envio CLIPPED,"/",
                     HOY USING"YYYYMMDD",".21UN",vfolio USING"&&&"
    RUN cat

    LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/CZACONF "
    RUN borra
    LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/DETCONF "
    RUN borra
    LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/DETCONF1 "
    RUN borra
    LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/SUMCONF "
    RUN borra

END FUNCTION
##############################################
FUNCTION intra()

     DEFINE   nss_intra  CHAR(11)
     DEFINE   movimiento CHAR(02)

     DECLARE cur_9 CURSOR FOR
         SELECT nss_uni,
		ident_movimiento
         FROM   uni_unificador
         WHERE  folio         = vfolio
         AND    cve_afo_recep = vcodigo_afore
    
     FOREACH cur_9 INTO nss_intra,movimiento

         SELECT "X"
         FROM   uni_unificado
         WHERE  folio        = vfolio
	 AND    nss_uni      = nss_intra
         AND    diag_unifica = "01"
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
	    CASE movimiento
	        WHEN "01"
                     UPDATE uni_unificado
                     SET    estado          = vintrafore,
                            estado_traspaso = 1
                     WHERE  nss_uni         = nss_intra
	             AND    folio           = vfolio
	             AND    estado = 20
 
                     UPDATE uni_unificador
                     SET    estado          = vintrafore,
                            estado_traspaso = 1
                     WHERE  nss_uni         = nss_intra
	             AND    folio           = vfolio
	             AND    estado = 20

	        WHEN "02"
		
                     UPDATE uni_unificado
                     SET    estado          = 25,
                            estado_traspaso = 1
                     WHERE  nss_uni         = nss_intra
	             AND    folio           = vfolio
	             AND    estado = 20
 
                     UPDATE uni_unificador
                     SET    estado          = 25,
                            estado_traspaso = 1
                     WHERE  nss_uni         = nss_intra
	             AND    folio           = vfolio
	             AND    estado = 20

             END CASE
         END IF
    END FOREACH

    UPDATE uni_cza_notifica
    SET    estado = vaceptado
    WHERE  folio  = vfolio

    UPDATE uni_sum_notifica
    SET    estado = vaceptado
    WHERE  folio  = vfolio

END FUNCTION
##############################################
FUNCTION improcedente()

     DEFINE   nss_impro CHAR(11)

     DECLARE cur_11 CURSOR FOR
         SELECT nss_uni
         FROM   uni_unificador
         WHERE  folio            = vfolio
                                                         -- JGHM Miguel 
                                                         -- Esto no debe existir 
--       AND    cve_afo_recep    = "000"                 -- 7 Dic 2009
	 AND    estado           = vconfronta
    
     FOREACH cur_11 INTO nss_impro

         SELECT "X"
         FROM   uni_unificado
         WHERE  folio        = vfolio
	 AND    nss_uni      = nss_impro
         AND    diag_unifica in("02","04","05","06")
	 AND    estado       = vconfronta
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
             UPDATE uni_unificado
             SET    estado          = vimprocede,
                    estado_traspaso = 1
             WHERE  nss_uni         = nss_impro
	     AND    folio           = vfolio
	     AND    estado          = vconfronta
 
             UPDATE uni_unificador
             SET    estado          = vimprocede,
                    estado_traspaso = 1
             WHERE  nss_uni         = nss_impro
	     AND    folio           = vfolio
	     AND    estado          = vconfronta

             SELECT "X"
             FROM   uni_unificado
             WHERE  folio        = vfolio
             AND    nss_uni      = nss_impro
             AND    diag_unifica <> "02"
	     AND    estado       = vimprocede
             GROUP BY 1

             IF STATUS <> NOTFOUND THEN
                 SELECT "X"
                 FROM   uni_recicla
                 WHERE  nss_uni   = nss_impro
                 GROUP BY 1

                 IF STATUS = NOTFOUND THEN
                     INSERT INTO uni_recicla VALUES(vfolio,nss_impro,HOY)
                 END IF
             END IF
         END IF
    END FOREACH
END FUNCTION
##############################################
FUNCTION cedente()
     DEFINE   nss_cede  CHAR(11)
     DEFINE   movimiento CHAR(02)

     DECLARE cur_10 CURSOR FOR
         SELECT nss_uni
         FROM   uni_unificador
         WHERE  folio         = vfolio
         AND    cve_afo_recep <> vcodigo_afore
         AND    cve_afo_recep <> "000"
         AND    estado        =  vconfronta

     FOREACH cur_10 INTO nss_cede

         SELECT "X"
         FROM   uni_unificado
         WHERE  folio        = vfolio
	 AND    nss_uni      = nss_cede
         AND    diag_unifica = "01"
         AND    estado       = vconfronta
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
             UPDATE uni_unificado
             SET    estado          = vcedente,
                    estado_traspaso = 1
             WHERE  nss_uni         = nss_cede
	     AND    folio           = vfolio
	     AND    estado          = vconfronta
 
             UPDATE uni_unificador
             SET    estado          = vcedente,
                    estado_traspaso = 1
             WHERE  nss_uni         = nss_cede
	     AND    folio           = vfolio
	     AND    estado          = vconfronta

         END IF
    END FOREACH

END FUNCTION
##############################################
REPORT listado_2(reg_2)

    DEFINE reg_2    RECORD LIKE uni_unificador.*
    DEFINE reg_3    RECORD LIKE uni_unificado.*
    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
    LET cont4 = cont4 +1
        PRINT 
            COLUMN 001,"02",#tipo_registro
            COLUMN 003,cont USING"&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,reg_2.tipo_ent_uni,
            COLUMN 015,reg_2.cve_ent_uni,
            COLUMN 018,reg_2.tipo_ent_nss,
            COLUMN 020,reg_2.cve_ent_nss,
            COLUMN 023,reg_2.curp_uni,
            COLUMN 041,reg_2.nss_uni,
            COLUMN 052,reg_2.rfc_uni,
            COLUMN 065,reg_2.paterno_uni,
            COLUMN 105,reg_2.materno_uni,
            COLUMN 145,reg_2.nombre_uni,
            COLUMN 185,reg_2.nombre_imss_uni,
            COLUMN 235,reg_2.sexo_uni,
            COLUMN 236,reg_2.ent_nac_uni,
            COLUMN 238,reg_2.fecha_nac_uni USING "yyyymmdd", 
            COLUMN 246,reg_2.tipo_documento,
            COLUMN 247,reg_2.cve_afo_recep USING "&&&",
            COLUMN 250,reg_2.num_ctas_asoc using "&&",
            COLUMN 268,reg_2.status_convoca,
            COLUMN 286,reg_2.ident_movimiento,
            COLUMN 288,reg_2.status_tra_nss,
            COLUMN 290,reg_2.status_ret_nss,
            COLUMN 292,reg_2.cve_afo_aclara,
	    COLUMN 295,reg_2.id_credito_43,
            COLUMN 296,35 SPACES

    OPEN cur_3 
      USING reg_2.folio,
	    reg_2.nss_uni
     WHILE TRUE
        FETCH cur_3 INTO reg_3.*

        LET cont = cont + 1

        IF STATUS = NOTFOUND THEN
           CLOSE cur_3
        LET cont = cont - 1
        LET cont1 = cont1 + 1
           EXIT WHILE
        END IF
	LET cont5 = cont5 +1
        PRINT 
            COLUMN 001,"03",#tipo_registro
            COLUMN 003,cont USING"&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,reg_3.nss_uni,
            COLUMN 024,reg_3.tipo_ent_cta1,
            COLUMN 026,reg_3.cve_ent_cta1,
            COLUMN 029,reg_3.curp_cta1,
            COLUMN 047,reg_3.nss_cta1,
            COLUMN 058,reg_3.rfc_cta1,
            COLUMN 071,reg_3.paterno_cta1,
            COLUMN 111,reg_3.materno_cta1,
            COLUMN 151,reg_3.nombre_cta1,
            COLUMN 191,reg_3.nombre_imss_cta1,
            COLUMN 241,reg_3.sexo_cta1,
            COLUMN 242,reg_3.ent_nac_cta1,
            COLUMN 244,reg_3.fecha_nac_cta1 USING "yyyymmdd",
            COLUMN 252,reg_3.status_tra_cta1,
            COLUMN 254,reg_3.status_ret_cta1,
            COLUMN 256,reg_3.status_convoca,
            COLUMN 257,reg_3.nss_cta2,
            COLUMN 268,reg_3.diag_unifica USING "&&",
            COLUMN 287,reg_3.cve_afo_aclara,
	    COLUMN 290,reg_3.id_credito_43_cta1,
            COLUMN 291,40 SPACES
     END WHILE
END REPORT
##############################################
REPORT listado_6(reg_6)
    DEFINE reg_6    RECORD LIKE uni_unificador.*
    DEFINE reg_7    RECORD LIKE uni_unificado.*

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW

    LET cont4 = cont4 +1
        PRINT 
            COLUMN 001,"02",#tipo_registro
            COLUMN 003,cont + cont2 USING"&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,reg_6.tipo_ent_uni,
            COLUMN 015,reg_6.cve_ent_uni,
            COLUMN 018,reg_6.tipo_ent_nss,
            COLUMN 020,reg_6.cve_ent_nss,
            COLUMN 023,reg_6.curp_uni,
            COLUMN 041,reg_6.nss_uni,
            COLUMN 052,reg_6.rfc_uni,
            COLUMN 065,reg_6.paterno_uni,
            COLUMN 105,reg_6.materno_uni,
            COLUMN 145,reg_6.nombre_uni,
            COLUMN 185,reg_6.nombre_imss_uni,
            COLUMN 235,reg_6.sexo_uni,
            COLUMN 236,reg_6.ent_nac_uni,
            COLUMN 238,reg_6.fecha_nac_uni USING "yyyymmdd", 
            COLUMN 246,reg_6.tipo_documento,
            COLUMN 247,reg_6.cve_afo_recep USING "&&&",
            COLUMN 250,reg_6.num_ctas_asoc using "&&",
            COLUMN 268,reg_6.status_convoca,
            COLUMN 286,reg_6.ident_movimiento,
            COLUMN 288,reg_6.status_tra_nss,
            COLUMN 290,reg_6.status_ret_nss,
            COLUMN 292,reg_6.cve_afo_aclara,
	    COLUMN 295,reg_6.id_credito_43,
            COLUMN 296,35 SPACES

    OPEN cur_8 
      USING reg_6.folio,
	    reg_6.nss_uni
     WHILE TRUE
        FETCH cur_8 INTO reg_7.*

        LET cont2 = cont2 + 1

        IF STATUS = NOTFOUND THEN
           CLOSE cur_8

        LET cont2 = cont2 - 1
        LET cont3 = cont3 + 1

           EXIT WHILE
        END IF
	LET cont5 = cont5 +1
        PRINT 
            COLUMN 001,"03",#tipo_registro
            COLUMN 003,cont + cont2 USING"&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,reg_7.nss_uni,
            COLUMN 024,reg_7.tipo_ent_cta1,
            COLUMN 026,reg_7.cve_ent_cta1,
            COLUMN 029,reg_7.curp_cta1,
            COLUMN 047,reg_7.nss_cta1,
            COLUMN 058,reg_7.rfc_cta1,
            COLUMN 071,reg_7.paterno_cta1,
            COLUMN 111,reg_7.materno_cta1,
            COLUMN 151,reg_7.nombre_cta1,
            COLUMN 191,reg_7.nombre_imss_cta1,
            COLUMN 241,reg_7.sexo_cta1,
            COLUMN 242,reg_7.ent_nac_cta1,
            COLUMN 244,reg_7.fecha_nac_cta1 USING "yyyymmdd",
            COLUMN 252,reg_7.status_tra_cta1,
            COLUMN 254,reg_7.status_ret_cta1,
            COLUMN 256,reg_7.status_convoca,
            COLUMN 257,reg_7.nss_cta2,
            COLUMN 268,reg_7.diag_unifica USING "&&",
            COLUMN 287,reg_7.cve_afo_aclara,
	    COLUMN 290,reg_7.id_credito_43_cta1,
            COLUMN 291,40 SPACES
     END WHILE
END REPORT
##############################################
REPORT listado_4(reg_4)
    DEFINE reg_4    RECORD LIKE uni_cza_notifica.*

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,reg_4.tipo_registro,#tipo_registro
            COLUMN 003,reg_4.ident_servicio,
            COLUMN 005,reg_4.ident_operacion,
            COLUMN 007,reg_4.tipo_ent_origen,
            COLUMN 009,reg_4.cve_ent_origen,
            COLUMN 012,reg_4.tipo_ent_destino,
            COLUMN 014,reg_4.cve_ent_destino,
            COLUMN 017,HOY USING "YYYYMMDD",
            COLUMN 025,reg_4.consec_lote USING "&&&",
            COLUMN 028, 303 SPACES

END REPORT
##############################################
REPORT listado_5(reg_5)

    DEFINE reg_5    RECORD LIKE uni_sum_notifica.*

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
            COLUMN 003,cont+cont2  USING "&&&&&&&&&" ,#cont_servicio ,
            COLUMN 012,cont4 USING "&&&&&&&&&",
            COLUMN 021,cont5 USING "&&&&&&&&&",
            COLUMN 030,301 SPACES

END REPORT
##############################################
FUNCTION actualiza_registrado()
   UPDATE uni_unificado
   SET    estado          = 40,
          estado_traspaso = 1
   WHERE  estado = 25
   AND    cve_ent_cta1 = vcodigo_afore
   AND    tipo_ent_cta1 = "01"
   AND    nss_uni in (select a.nss_uni
                      from   uni_unificador a
                      WHERE  a.cve_afo_recep = vcodigo_afore
                      AND    a.estado IN (25,40))

   UPDATE uni_unificador
   SET    estado          = 40,
          estado_traspaso = 1
   WHERE  estado = 25
   AND    cve_ent_nss = vcodigo_afore
   AND    tipo_ent_nss = "01"
   AND    cve_afo_recep = vcodigo_afore

END FUNCTION
