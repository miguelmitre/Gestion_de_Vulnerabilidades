#############################################################################
#Proyecto          => SISTEMA DE AFORES ( MEXICO )                            #
#Programa PROC016  => RECIBE RESPUESTA DE RESULTADO DE EXAMEN                 #
#Modulo            => PRO                                                     #
#Elaborado Por     => STEFANIE DANIELA VERA PIÑA                              #
#Fecha Elaboracion => 27-ABRIL-2007                                           #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE 
       reg                   RECORD LIKE pro_cza_examen.*,
       reg1                  RECORD LIKE pro_ctr_examen.*

    DEFINE  #s_modulo
       s_modulo              RECORD LIKE seg_modulo.*    

    DEFINE reg_2 RECORD  #glo #reg_2
       nom_archivo           CHAR(20)
    END RECORD

    DEFINE #glo INTEGER
       cont                  ,
       cuantos               ,
       cont_det              ,
       ultimo_folio          INTEGER

    DEFINE #glo #char
       archivo_examen        CHAR(200),
       enter                 CHAR(1)  ,
       usuario               CHAR(12) ,
       c10_fecha             CHAR(10),
       vcve_solicitud        CHAR(13),
       carga_reg             CHAR(520)
 
    DEFINE #glo SMALLINT
       sw_1                  ,
       s_codigo_afore        ,
       s_lote_generado       ,
       s_actualizado         ,
       vcalif                SMALLINT

    DEFINE #glo DATE 
       HOY                   DATE

END GLOBALS


MAIN
     OPTIONS
        PROMPT LINE LAST
        DEFER INTERRUPT
        CALL STARTLOG("PROC016.log")
        WHENEVER ERROR CONTINUE

     CREATE TEMP TABLE ret_pla_carga
        (
         n_registros          CHAR(520)
        )
     WHENEVER ERROR STOP

     CALL init()
     OPEN WINDOW proc0161 AT 4,4 WITH FORM "PROC0161" ATTRIBUTE(BORDER)
     DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
     DISPLAY " PROC016  RECIBE RESP.ARCHIVO DE ENVIO DE SOLICITUDES DE EXAMEN               " AT 3,1 ATTRIBUTE(REVERSE)
     DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

     INPUT BY NAME reg_2.nom_archivo WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo
           LET reg_2.nom_archivo = NULL
           CLEAR FORM

        AFTER FIELD nom_archivo
           IF reg_2.nom_archivo IS NULL THEN
	      ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	      NEXT FIELD nom_archivo
	   END IF

           SELECT "OK"
           FROM   pro_cza_examen
           WHERE  nom_archivo = reg_2.nom_archivo
           GROUP BY 1

           IF STATUS <> NOTFOUND THEN
              ERROR "   ARCHIVO YA PROCESADO CON ANTERIORIDAD" ATTRIBUTE(NORMAL)
              NEXT FIELD nom_archivo
           END IF

           WHENEVER ERROR CONTINUE
           SELECT *
           INTO   s_modulo.*
           FROM   seg_modulo
           WHERE  modulo_cod = "pro"

           LET archivo_examen = s_modulo.ruta_rescate CLIPPED,"/",
                                reg_2.nom_archivo CLIPPED

           LOAD FROM archivo_examen 
           INSERT INTO ret_pla_carga
                      
           SELECT count(*)
           INTO   cuantos
           FROM   ret_pla_carga
                    
           IF cuantos = 0 THEN
              ERROR "   NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
              ATTRIBUTE(NORMAL)
              NEXT FIELD nom_archivo
           ELSE
              EXIT INPUT
           END IF
           WHENEVER ERROR STOP

        ON KEY (INTERRUPT)
             PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
             EXIT PROGRAM
     END INPUT

     DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

     SELECT MAX(folio) + 1
     INTO   ultimo_folio
     FROM   glo_folio

     IF ultimo_folio IS NULL THEN
         LET ultimo_folio = 1
     END IF

     INSERT INTO glo_folio VALUES(ultimo_folio)
       
     DISPLAY "FOLIO NUMERO  : ",ultimo_folio  AT 18,2

     CALL primer_paso()

     PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
     FOR CHAR enter

     CLOSE WINDOW proc0161
END MAIN


FUNCTION init()
#--------------
    LET HOY  = TODAY
    LET sw_1 = 0

    SELECT A.status_interno
    INTO   s_lote_generado
    FROM   pro_status_interno A
    WHERE  A.desc_status_corta = "LOTE GENERADO"

    SELECT A.status_interno
    INTO   s_actualizado
    FROM   pro_status_interno A
    WHERE  A.desc_status_corta = "ACTUALIZADO"

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local    
END FUNCTION


FUNCTION primer_paso()
#---------------------
    LET cont = 0
    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    ret_pla_carga

    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1


            LET reg1.cve_solicitud = carga_reg[1,13] 
            LET reg1.tpo_examen    = carga_reg[14,14]
            LET reg1.evento        = carga_reg[15,15]
         

            LET c10_fecha = carga_reg[18,19],"/", 
                            carga_reg[16,17],"/",
                            carga_reg[20,23]

            LET reg1.fecha_recep = c10_fecha
            LET reg1.hora_examen =  carga_reg[24,25],":",
                                    carga_reg[26,27]

            LET reg1.calif       = carga_reg[28,30]
            LET reg1.result      = carga_reg[31,31] 

        IF cont = 1 THEN   
            
            SELECT "OK"
            FROM   pro_cza_examen
            WHERE  nom_archivo = reg_2.nom_archivo

        
            IF STATUS = NOTFOUND THEN
               INSERT INTO pro_cza_examen
               VALUES( ultimo_folio                  ,
                       reg1.fecha_recep               ,
                       reg_2.nom_archivo             ,
		       HOY                           ,# fecha_carga
                       0             
                      )
            ELSE
                DELETE
                FROM  glo_folio
                WHERE folio = ultimo_folio

                DISPLAY ""  AT 18,2
                PROMPT " ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER>",
                       " PARA SALIR" FOR CHAR enter
                EXIT PROGRAM
            END IF
         END IF


            LET cont_det = cont_det + 1

            DISPLAY " TOTAL REGISTROS DE DETALLE     : ",cont_det AT 11,8

            UPDATE pro_ctr_examen
            SET    calif           = reg1.calif,
                   result          = reg1.result,
                   folio_recep     = ultimo_folio,
                   fecha_recep     = HOY,
                   estado_registro = s_actualizado
            WHERE  cve_solicitud = reg1.cve_solicitud
              AND  estado_registro = s_lote_generado

            UPDATE pro_cza_examen
            SET    tot_registros = cont   
            WHERE  folio = ultimo_folio
            AND    fecha_info = reg1.fecha_recep
            AND    nom_archivo = reg_2.nom_archivo
            AND    fecha_carga = HOY

    END FOREACH
END FUNCTION
