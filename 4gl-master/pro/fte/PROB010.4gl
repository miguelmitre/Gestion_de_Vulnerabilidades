################################################################################
#Proyecto          => SISTEMA DE safre_af ( MEXICO )                           #
#Sistema           => PRO. 	                                               #
#Programa PROB010  => TRASPASA PREPROMOTORES AL MAESTRO DE PROMOTORES          #
#Fecha creacion    => 07 DE FEBRERO DEL 2000                                   #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Elaboracion => 12 DE ENERO DEL 2001                                     #
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN                              #
#Fecha actualiz.   => 29 DE Marzo DEL 2004                                     #
################################################################################
DATABASE safre_af
GLOBALS                                    
    DEFINE #glo #reg_1 #reg_2
        reg_1                 RECORD LIKE pro_solicitud.* ,
        reg_2                 RECORD LIKE pro_mae_promotor.*
    
    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(1) ,
        aux_pregunta          CHAR(1) 

    DEFINE #glo #smallint
	num_reg 	      ,
	num_reg2 	      ,
        nro_de_registros     SMALLINT

    DEFINE #glo #integer
        nn                    INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PROB010.log")

    CALL init() #i
    OPEN WINDOW prob0101  AT 4,4 WITH FORM "PROB0101" ATTRIBUTE(BORDER)
    DISPLAY "                             <Ctrl-C> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROB010            TRASPASA AL MAESTRO DE PROMOTORES                           " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
        
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_1.folio WITHOUT DEFAULTS
        AFTER FIELD folio
            IF reg_1.folio IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   pro_solicitud A
            WHERE  A.folio = reg_1.folio
            GROUP BY 1
 
            IF STATUS = NOTFOUND THEN
                ERROR " FOLIO INEXISTENTE "
                NEXT FIELD folio
            ELSE
                SELECT "OK"
                FROM   pro_solicitud A
                WHERE  A.folio          = reg_1.folio
                AND    A.status_interno IN(4,40,41)
                AND    A.diag_proceso IN ("1A","1R","1K","4A")
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    PROMPT " NO HAY REGISTROS PARA TRASPASAR...<ENTER> PARA",
                           " SALIR " FOR CHAR enter
                    EXIT PROGRAM
                END IF
            END IF

        ON KEY (ESC)
            IF reg_1.folio IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   pro_solicitud A
            WHERE  A.folio = reg_1.folio
            GROUP BY 1
 
            IF STATUS = NOTFOUND THEN
                ERROR " FOLIO INEXISTENTE "
                NEXT FIELD folio
            ELSE
                SELECT "OK"
                FROM   pro_solicitud A
                WHERE  A.folio          = reg_1.folio
                AND    A.status_interno IN(4,40,41)
                AND    A.diag_proceso IN ("1A","1R","1K","4A")
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    PROMPT " NO HAY REGISTROS PARA TRASPASAR...<ENTER> PARA",
                           " SALIR " FOR CHAR enter
                    EXIT PROGRAM
                END IF
            END IF

            WHILE TRUE
                PROMPT " ESTA SEGURO DE QUERER ACTUALIZAR MAESTRO S/N "
                FOR CHAR enter 
                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Nn]" THEN
                        PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
                        FOR CHAR enter
                        EXIT PROGRAM
                    ELSE
                        EXIT WHILE
                    END IF
                END IF
            END WHILE

            EXIT INPUT
  
        ON KEY (INTERRUPT,CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    CALL primer_paso() #pp

    PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter
    DISPLAY "" AT 19,1
    CLOSE WINDOW prob0101
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT MAX(folio)
    INTO   reg_1.folio
    FROM   pro_solicitud A
    WHERE  A.status_interno IN(4,40,41)
    AND    A.diag_proceso   IN ("1A","1R","1K","4A")
END FUNCTION 

FUNCTION primer_paso()
#pp-------------------
    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    pro_solicitud A
    WHERE   A.folio          = reg_1.folio
    AND     A.status_interno IN(4,40,41)
    AND     A.diag_proceso IN ("1A","1R","1K","4A")
    and     A.cod_promotor is not null --condicion momentanea hasta resolver
                                       --duplicidad

    LET num_reg  = 0
    FOREACH cur_1 INTO reg_1.*
        SELECT a.*
        INTO   reg_2.*
        FROM   pro_mae_promotor a
        WHERE  a.cod_promotor = reg_1.cod_promotor
        OR     a.codven       = reg_1.codven
           
        IF SQLCA.SQLCODE <> 0 THEN
            SELECT a.*
            INTO   reg_2.*
            FROM   pro_mae_promotor a
            WHERE  a.paterno = reg_1.paterno
            AND    a.materno = reg_1.materno
            AND    a.nombres = reg_1.nombres

            IF SQLCA.SQLCODE <> 0 THEN
                CALL cuarto_paso(reg_1.*) #cp
            ELSE
                CALL tercer_paso(reg_1.*,reg_2.*)#tp
            END IF
        ELSE
            CALL tercer_paso(reg_1.*,reg_2.*)#tp
        END IF
        LET num_reg = num_reg + 1
    END FOREACH

    DISPLAY " SE TRASPASARON : ",num_reg USING "#########&", " REGISTROS "
    AT  17,2
END FUNCTION

FUNCTION segundo_paso()
#sp--------------------
    UPDATE pro_mae_promotor
    SET    pro_mae_promotor.rfc             = reg_1.rfc            ,
           pro_mae_promotor.unico           = reg_1.unico          ,
           pro_mae_promotor.paterno         = reg_1.paterno        ,
           pro_mae_promotor.materno         = reg_1.materno        ,
           pro_mae_promotor.nombres         = reg_1.nombres        ,
           pro_mae_promotor.fingre          = reg_1.fingre         ,
           pro_mae_promotor.fenvio          = reg_1.fenvio         ,
           pro_mae_promotor.fecha_registro  = reg_1.fecha_registro ,
           pro_mae_promotor.fecha_baja      = NULL                 ,
           pro_mae_promotor.status          = 1                    ,
           pro_mae_promotor.nro_solicitud   = reg_1.nro_solicitud  ,
           pro_mae_promotor.motivo_suspende = ""                   ,
           pro_mae_promotor.fecha_suspende  = ""                   ,
           pro_mae_promotor.status_interno  = 5                    ,
           pro_mae_promotor.diag_proceso    = reg_1.diag_proceso   ,
           pro_mae_promotor.tipo_recibo     = reg_1.tipo_recibo    ,
           pro_mae_promotor.cod_promotor    = reg_1.cod_promotor
--    WHERE  pro_mae_promotor.cod_promotor    = reg_1.cod_promotor
    WHERE  pro_mae_promotor.codven          = reg_1.codven

    UPDATE pro_solicitud 
    SET    pro_solicitud.status_interno = 5
    WHERE  pro_solicitud.folio        = reg_1.folio
    AND    pro_solicitud.cod_promotor = reg_1.cod_promotor
END FUNCTION

FUNCTION tercer_paso(reg_1,reg_2)
#tp------------------------------
    DEFINE #loc #reg_2
        reg_1                 RECORD LIKE pro_solicitud.*  ,
        reg_2                 RECORD LIKE pro_mae_promotor.*

    CASE reg_2.status
        WHEN 1
            IF reg_1.diag_proceso = "4A" THEN
                INSERT INTO pro_his_revalida VALUES(reg_1.folio,reg_2.*)
                CALL segundo_paso() #sp
            ELSE
                   CALL cuarto_paso(reg_1.*) #cp
            END IF
        WHEN 2
            SELECT 'a.X' FROM pro_his_baja a
            WHERE a.cod_promotor = reg_2.cod_promotor
            OR    a.codven       = reg_2.codven      

            IF SQLCA.SQLCODE = 0 THEN
               DELETE FROM pro_his_baja
               WHERE  pro_his_baja.cod_promotor = reg_2.cod_promotor
            END IF

            INSERT INTO pro_his_baja VALUES(reg_1.folio,reg_2.*)

            DELETE
            FROM   pro_mae_promotor
            WHERE  pro_mae_promotor.cod_promotor = reg_2.cod_promotor
            
            CALL cuarto_paso(reg_1.*) #cp
        WHEN 3
            SELECT 'a.X' FROM pro_his_baja a
            WHERE a.cod_promotor = reg_2.cod_promotor
            OR    a.codven = reg_2.codven

            IF SQLCA.SQLCODE = 0 THEN
               DELETE FROM pro_his_baja      
               WHERE pro_his_baja.cod_promotor = reg_2.cod_promotor
               OR    pro_his_baja.codven = reg_2.codven
            END IF

            INSERT INTO pro_his_baja VALUES(reg_1.folio,reg_2.*)
            CALL segundo_paso() #sp
    END CASE
END FUNCTION

FUNCTION cuarto_paso(reg_1)
#cp------------------------
    DEFINE #loc #reg_1
        reg_1                 RECORD LIKE pro_solicitud.* ,

        estructura  RECORD
           agenc_cod   LIKE pro_mae_promotor.agenc_cod,
           nip         LIKE pro_mae_promotor.nip,
           nivel       LIKE pro_mae_promotor.nivel
        END RECORD

    INITIALIZE estructura.* TO NULL

    SELECT a.agenc_cod, a.nip, a.nivel
    INTO   estructura.*
    FROM pro_mae_promotor a
    WHERE  a.cod_promotor = reg_1.cod_promotor
    OR     a.codven       = reg_1.codven

    IF SQLCA.SQLCODE <> 0 THEN

       DELETE FROM pro_mae_promotor
       WHERE pro_mae_promotor.cod_promotor = reg_1.cod_promotor

       INSERT INTO pro_mae_promotor
       VALUES(reg_1.codven         ,
              reg_1.seguro         ,
              reg_1.nip            ,
              reg_1.agenc_cod      ,
              reg_1.unico          ,
              reg_1.rfc            ,
              reg_1.paterno        ,
              reg_1.materno        ,
              reg_1.nombres        ,
              reg_1.fingre         ,
              reg_1.fenvio         ,
              reg_1.fecha_registro ,
              ""                   ,#fecha_baja
              reg_1.calle          ,
              reg_1.numero         ,
              reg_1.dpto           ,
              reg_1.colonia        ,
              reg_1.deleg          ,
              reg_1.ciudad         ,
              reg_1.estado         ,
              reg_1.codpos         ,
              reg_1.fono           ,
              reg_1.sup            ,
              reg_1.nivel          ,
              reg_1.resuelva       ,
              reg_1.fnaci          ,
              reg_1.diag_proceso   ,
              ""                   ,#fautoriz
              1                    ,#status
              reg_1.nro_solicitud  ,
              4                    ,#status_interno
              ""                   ,#fecha_certifi
              " "                  ,#motivo_suspende
              " "                  ,#fecha_suspende
              " "                  ,#fecha_credencial
              reg_1.cod_promotor   ,
              reg_1.tipo_recibo    ,#captura de comercial
              reg_1.escolar 
             )
                                        

    ELSE
       DELETE FROM pro_mae_promotor
       WHERE  pro_mae_promotor.cod_promotor = reg_1.cod_promotor

       INSERT INTO pro_mae_promotor
       VALUES(reg_1.codven         ,
              reg_1.seguro         ,
              estructura.nip            ,
              estructura.agenc_cod      ,
              reg_1.unico          ,
              reg_1.rfc            ,
              reg_1.paterno        ,
              reg_1.materno        ,
              reg_1.nombres        ,
              reg_1.fingre         ,
              reg_1.fenvio         ,
              reg_1.fecha_registro ,
              ""                   ,#fecha_baja
              reg_1.calle          ,
              reg_1.numero         ,
              reg_1.dpto           ,
              reg_1.colonia        ,
              reg_1.deleg          ,
              reg_1.ciudad         ,
              reg_1.estado         ,
              reg_1.codpos         ,
              reg_1.fono           ,
              reg_1.sup            ,
              estructura.nivel          ,
              reg_1.resuelva       ,
              reg_1.fnaci          ,
              reg_1.diag_proceso   ,
              ""                   ,#fautoriz
              1                    ,#status
              reg_1.nro_solicitud  ,
              4                    ,#status_interno
              ""                   ,#fecha_certifi
              " "                  ,#motivo_suspende
              " "                  ,#fecha_suspende
              " "                  ,#fecha_credencial
              reg_1.cod_promotor   ,
              reg_1.tipo_recibo    ,#captura de comercial
              reg_1.escolar 
             )
    END IF

    UPDATE pro_solicitud 
    SET    pro_solicitud.status_interno = 5
    WHERE  pro_solicitud.folio          = reg_1.folio
    AND    pro_solicitud.cod_promotor   = reg_1.cod_promotor
END FUNCTION
