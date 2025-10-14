#############################################################################
#Proyecto          => SISTEMA DE AFORES ( MEXICO )                          #
#Programa PROC0141 => REVERSO DE PROCESO 0803                               #
#Modulo            => PRO                                                   #
#Elaborado Por     => ISABEL FONSECA FRIAS                                  #
#Fecha Elaboracion => 03-Septiembre-2008                                    #
#############################################################################
DATABASE safre_af
GLOBALS
    DEFINE 
       reg                   RECORD LIKE pro_cza_examen.*,
       reg1                  RECORD LIKE pro_ctr_examen.*

    DEFINE  #s_modulo
       g_seg_modulo          RECORD LIKE seg_modulo.*  

    DEFINE reg_2 RECORD  #glo #reg_2
       fecha                DATE 
    END RECORD

    DEFINE #glo INTEGER
       cont                  ,
       cuantos               ,
       cont_det              ,
       ultimo_folio          INTEGER

    DEFINE #glo #char
       archivo_examen        CHAR(200),
       elimina               CHAR(500) ,
       enter                 CHAR(1)  ,
       usuario               CHAR(12) ,
       c10_fecha             CHAR(10),
       vcve_solicitud        CHAR(13),
       v_nom_archivo         CHAR(30),
       v_folio_resexamen     INTEGER,   
       carga_reg             CHAR(520)
 
    DEFINE #glo SMALLINT
       sw_1                  ,
       s_codigo_afore        ,
       s_actualizado         ,
       s_operacion_generada  ,
       vcalif                SMALLINT

    DEFINE #glo DATE 
       HOY                   DATE

END GLOBALS


MAIN
     OPTIONS
        PROMPT LINE LAST
        DEFER INTERRUPT
        CALL STARTLOG("PROC0141.log")
        WHENEVER ERROR CONTINUE

        SELECT *
        INTO   g_seg_modulo.*
        FROM   seg_modulo
        WHERE  modulo_cod = "pro"


     CALL init()
     OPEN WINDOW proc01411 AT 4,4 WITH FORM "PROC01411" ATTRIBUTE(BORDER)
     DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
     DISPLAY " PROC0141                REVERSO DE OPERACION 0803                              " AT 3,1 ATTRIBUTE(REVERSE)
     DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

     INPUT BY NAME reg_2.fecha WITHOUT DEFAULTS
        BEFORE FIELD fecha 
           LET reg_2.fecha = NULL
           CLEAR FORM

        AFTER FIELD fecha 
           IF reg_2.fecha IS NULL THEN
	      ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	      NEXT FIELD fecha 
	   END IF

           SELECT "OK"
           FROM   pro_ctr_examen
           WHERE  fecha_envioexam = reg_2.fecha
             AND  estado_registro = s_operacion_generada
           GROUP BY 1


           IF STATUS = NOTFOUND THEN
              ERROR "NO EXISTEN REGISTROS A REVERSAR" ATTRIBUTE(NORMAL)
              NEXT FIELD fecha 
           ELSE

              LET v_folio_resexamen = NULL
 
              SELECT unique(folio_resexamen)
                INTO v_folio_resexamen 
                FROM pro_ctr_examen
              WHERE  fecha_envioexam = reg_2.fecha
                AND  estado_registro = s_operacion_generada 

 
              LET v_nom_archivo = NULL

              SELECT nom_archivo
                INTO v_nom_archivo 
                FROM pro_envio_examen
               WHERE folio = v_folio_resexamen 
                 AND fecha_info = reg_2.fecha 
 
              IF v_nom_archivo is null THEN
                 ERROR "No existe archivo a rebersar con la fecha capturada"
                 SLEEP 4 
                 NEXT FIELD fecha
              ELSE
                 DISPLAY  "Archivo a Reversar:    ", v_nom_archivo AT 15,05 
                 DISPLAY  "Folio             :    ", v_folio_resexamen AT 16,05 
              END IF
           
           END IF

        ON KEY (INTERRUPT)
             PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
             EXIT PROGRAM
     END INPUT

        PROMPT " ESTA SEGURO DE REVERSAR.... S/N " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN


               DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)


               -- Actualiza informacion de pro_ctr_examen
               UPDATE pro_ctr_examen SET
                      folio_resexamen = 0,
                      fecha_envioexam = '',
                      estado_registro = s_actualizado
              WHERE  fecha_envioexam = reg_2.fecha
                AND  estado_registro = s_operacion_generada
                AND  folio_resexamen = v_folio_resexamen

              -- Borra archivo de pro_envio_exam
              DELETE 
              FROM pro_envio_examen
              WHERE fecha_info = reg_2.fecha 
                AND folio =  v_folio_resexamen

              -- Borra archivo plano de la ruta envio
              WHENEVER ERROR CONTINUE
              LET elimina = "rm -f ",g_seg_modulo.ruta_envio CLIPPED,"/",v_nom_archivo

              RUN elimina
              WHENEVER ERROR STOP

              DISPLAY "                        " AT 19,1 

              PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
              FOR CHAR enter

           END IF
        END IF

     CLOSE WINDOW PROC01411
END MAIN


FUNCTION init()
#--------------
    LET HOY  = TODAY
    LET sw_1 = 0

    SELECT A.status_interno
    INTO   s_actualizado
    FROM   pro_status_interno A
    WHERE  A.desc_status_corta = "ACTUALIZADO"

    SELECT A.status_interno
    INTO   s_operacion_generada
    FROM   pro_status_interno A
    WHERE  A.desc_status_corta = "OPERACION GENERADA"

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local    
END FUNCTION
