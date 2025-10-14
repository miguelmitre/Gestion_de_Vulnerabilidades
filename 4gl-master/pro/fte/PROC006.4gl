################################################################################
#Proyecto          => SISTEMA SAFRE ( MEXICO )                                 #
#Sistema           => PRO.                                                     #
#Programa PROC006  => ENVIO DE SOLICITUDES PARA LA REVALIDACION DE PROMOTORES  #
#Fecha             => 11 DE ENERO DE 2000                                      #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Modificacion=> 29 DE MARZO DEL 2004                                     #
#Modificado Por    => LAURA EUGENIA CORTES GUZMAN                              #
#Fecha Modificacion=> 01 de abril del 2008                                     #
#Modificado Por    => ISABEL FONSECA FRIAS                                     #
#Descripcion       => Se agrego en el UPDATE de pro_capacitacion en la         #
#                  => funcion primer_paso() el filtro por estado = 1 ya que    #
#                  => de lo contrario actualizaba todos los registros del pro- #
#                  => motor con el que se esta trabajando (v1)                 #
# CUO              => FSR 04/08/2015	SE ACTUALIZA A NUEVO LAYOUT V9.0         #
# CUO II           => FSR 29/03/2016  SE ACTUALIZA A NUEVO LAYOUT V10.0        #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE parametro     RECORD LIKE seg_modulo.*,

           reg_1 RECORD 
               cod_promotor      CHAR(10) ,
             #  nombres           CHAR(40) , #CPL-2295
             #  paterno           CHAR(40) , #CPL-2295
             #  materno           CHAR(40) , #CPL-2295
               unico             CHAR(18) ,
               calificacion      SMALLINT
             #  ind_vigencia      CHAR(1)   #CPL-2295
           END RECORD ,

           HOY                   DATE ,
           RUTA                  CHAR(100) ,
           G_LISTA               CHAR(100) ,
           enter                 CHAR(001) ,
           nro_reg_enviados      SMALLINT
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV("USER")||".PROC006.log")
    
    CALL init() #i
    OPEN WINDOW proc0061 AT 4,4 WITH FORM "PROC0061" ATTRIBUTE(BORDER)
    DISPLAY "                             <Ctrl-C> Sal",
            "ir                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC006          ENVIO DE PROMOTORES PARA ",
            "REVALIDACION                        " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                SELECT "OK"
                FROM   pro_capacitacion A, pro_mae_promotor B
                WHERE  A.estado       = 1
                #AND    A.horas       >= 40
                AND    A.cod_promotor = B.cod_promotor
                AND    B.status       = 1
                GROUP BY 1

                IF SQLCA.SQLCODE <> 0 THEN
                    PROMPT " NO EXISTEN REGISTROS PARA PROCESAR...<ENTER> ",
                           "PARA SALIR " FOR CHAR enter
                    EXIT PROGRAM
                END IF

                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter 
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
    CALL primer_paso() #pp

    DISPLAY " ARCHIVO GENERADO EN RUTA : ",RUTA AT 8,7
    DISPLAY " CON EL NOMBRE                        : ","REV" AT 10,7
    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW proc0061
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT *
    INTO   parametro.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pro"

    LET RUTA    = parametro.ruta_envio CLIPPED,"/"
    LET G_LISTA = RUTA CLIPPED,"REV"
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DECLARE cur_1 CURSOR FOR
        SELECT A.cod_promotor ,
               #B.nombres      , #CPL-2295
               #B.paterno      , #CPL-2295
               #B.materno      , #CPL-2295
               B.unico        ,  
               A.calificacion
               #A.ind_vigencia   #CPL-2295
        FROM   pro_capacitacion A, pro_mae_promotor B
        WHERE  A.cod_promotor = B.cod_promotor
        AND    A.estado        = 1
        AND    A.calificacion >= 75
        #AND    A.horas        >= 60

        START REPORT listado_1 TO G_LISTA
        LET nro_reg_enviados = 0
    FOREACH cur_1 INTO reg_1.*

        LET nro_reg_enviados = nro_reg_enviados + 1

        DISPLAY "NRO.DE REGISTROS PROCESADOS :",nro_reg_enviados AT 15,8

        OUTPUT TO REPORT listado_1(reg_1.*)

        UPDATE pro_capacitacion
        SET    estado       = 2 ,
               fecha_genera = HOY
        WHERE  cod_promotor = reg_1.cod_promotor
          AND  estado = 1                                      --(v1)

       #CPL-3604	se inserta informacion del envio de la certificacion del asesor previsional   
       INSERT INTO pro_certificado_prov
       VALUES (reg_1.cod_promotor, #cod_promotor
               ""                , #unico
               0                 , #folio  
               ""                , #fenvio
               ""                , #frecepcion
               "302"             , #operacion
               ""                , #fvigencia
               ""                , #diagnostico
               HOY               )   
                    
    END FOREACH

    FINISH REPORT listado_1

    INSERT INTO pro_ctr_envio
        VALUES (HOY              ,#fecha_genera
                "REV"            ,#tipo_operacion
                2                ,#estado                       
                ""               ,#fenvio
                nro_reg_enviados ,#nro_de_registros
                ""                #nro_lote
               )

END FUNCTION

REPORT listado_1(reg_1)
#l1--------------------
    
    DEFINE reg_1 RECORD #loc #reg_1
        cod_promotor      CHAR(10) ,
        #nombres           CHAR(40) ,
        #paterno           CHAR(40) ,
        #materno           CHAR(40) ,
        unico             CHAR(18) ,
        calificacion      SMALLINT
        #ind_vigencia      CHAR(1)
    END RECORD    

    OUTPUT
        LEFT   MARGIN 0
        RIGHT  MARGIN 0
        TOP    MARGIN 0
        BOTTOM MARGIN 0
        PAGE   LENGTH 1

    FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 001,"302"                                ,
            COLUMN 004,reg_1.cod_promotor                   , 
            COLUMN 014,reg_1.unico                          ,
            COLUMN 032,reg_1.calificacion USING "&&&"       ,
            COLUMN 035,"999"                                ,
            COLUMN 038,863 SPACES
END REPORT
