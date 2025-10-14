###############################################################################
#Proyecto          => SISTEMA DE AFORES ( MEXICO )                            #
#Modulo            => PRO                                                     #
#Programa PROM0031 => Reactivacion de Bajas y Revalidaciones                  #
#Por               => LAURA EUGENIA CORTES GUZMAN                             #
#Fecha             => 20 de Agosto del 2005                                   #
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN                             #
#Fecha actualiz.   => 20 de Agosto del 2005                                   #
###############################################################################

DATABASE safre_af
GLOBALS
    "PROM003.4gl"

FUNCTION reactiva_2()
#r-----------------

    LET pos = 2
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        LET HOY      = TODAY

        OPEN WINDOW pantalla4 AT 2,3 WITH FORM "PROM0032" ATTRIBUTE(BORDER)
        DISPLAY " PROM003      SOLICITUDES DE REGISTRO DE AGENTES ",
                "PROMOTORES                    " AT 2,1 ATTRIBUTE(REVERSE)
        DISPLAY "                     DOMICILIO   DE   CORRESPONDEN",
                "CIA                          " AT 14,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE
        DISPLAY " REACTIVA 2 " AT 1,66 ATTRIBUTE(NORMAL)
        DISPLAY "ESC: Consultar,  Ctrl-C: Salir  " AT 1,2

        WHILE TRUE
            CALL construc_reactiv2() #cr
            RETURNING salida

            IF salida = "S" THEN
                CALL inicializa()
                CLEAR FORM
            ELSE
                LET salida = NULL
                CALL inicializa()
                EXIT WHILE
            END IF
        END WHILE
    END IF

    CLOSE WINDOW pantalla4
    CLEAR SCREEN
END FUNCTION

FUNCTION construc_reactiv2()
#cr---------------------

    DEFINE 
        xxx        SMALLINT, 
        cont_reg              SMALLINT,
        i                     SMALLINT

    DEFINE #loc #char
        x_fecha               CHAR(10) ,
        desciuda              CHAR(18) ,
        dessup                CHAR(18) ,
        desnivel              CHAR(18) ,
        desdeleg              CHAR(18) ,
        aaa                   CHAR(02) ,
        z_fecha               CHAR(10) ,
        mm                    CHAR(02) ,
        dd                    CHAR(02) ,
        st_int                SMALLINT

    DEFINE #glo #date
        xx_fecha              DATE     ,
        j_fecha               DATE

    DEFINE reg1     RECORD
        folio                 INTEGER,
        agenc_cod             CHAR(10),
        status                SMALLINT,
        status_interno        SMALLINT,
        num_lote              INTEGER,
        nip                   SMALLINT,
        sup                   CHAR(10),
        nivel                 SMALLINT
    END RECORD,

        reg_mae    RECORD LIKE pro_mae_promotor.*

    DEFINE
        vrfc                  CHAR(13),
        vcodven               CHAR(10)
                                         
        INITIALIZE  reg_mae.* TO NULL

        CONSTRUCT cla_where ON   A.nro_solicitud,
                                 A.codven,
                                 A.cod_promotor,
                                 A.seguro,rfc,
                                 A.paterno,
                                 A.materno,
                                 A.nombres
                            FROM nro_solicitud,codven,cod_promotor,seguro,rfc,
                                 paterno,materno,nombres
            ON KEY (ESC)
                LET int_flag = FALSE
                EXIT CONSTRUCT
            ON KEY (CONTROL-C,INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT
         END CONSTRUCT

         IF int_flag = TRUE THEN
             LET int_flag = FALSE
             CLEAR SCREEN
             LET salida = "N"
             RETURN salida
         END IF

         LET sel_where = " SELECT UNIQUE A.status_interno,",
                         " E.desc_status_corta,",    
                         " A.nro_solicitud,",
                         " A.codven,",
                         " A.cod_promotor,",
                         " A.seguro,",
                         " A.rfc,",
                         " A.unico,",
                         " A.paterno,",
                         " A.materno,",
                         " A.nombres,",
                         " A.diag_proceso,",
                         " A.fnaci,",
                         " A.fecha_baja,",
                         " A.fingre,",
                         " A.fenvio,",
                         " A.fecha_registro,",
                         " A.fecha_proceso,",
                         " A.resuelva,",
                         " A.horas_capacit,",
##                         " A.tipo_recibo,",
                         " A.escolar    ,",
                         " A.calle,",
                         " A.numero,",
                         " A.dpto,",
                         " A.codpos,",
                         " A.colonia,",
                         " A.deleg,",
                         " B.deleg_desc,",
                         " A.ciudad,",
                         " C.ciudad_desc,",
                         " A.estado,",
                         " D.estad_desc,",
                         " A.fono ",
                         " FROM   pro_solicitud A, pro_mae_promotor F,",
                         " OUTER(tab_delegacion B, ",
                         " tab_ciudad C,tab_estado D,pro_status_interno E) ",
                         " WHERE  ",cla_where CLIPPED,
                         " AND    A.cod_promotor = F.cod_promotor ",
                         " AND   (F.diag_proceso IN('4B','4C',",
                         " '4D','4E','4G','4H','4I')",
                         " OR     F.motivo_suspende IN('6C','3E'))",
                         " AND    B.deleg_cod  = A.deleg ",
                         " AND    C.ciudad_cod   = A.ciudad ",
                         " AND    D.estad_cod  = A.estado ",
--                         " AND    E.status_interno = F.status_interno ",
                         " AND    A.status_interno <> 0 ",
                         " ORDER BY 3,6 "

         PREPARE query4a FROM sel_where

         DECLARE cursor_6a CURSOR FOR query4a

         LET pos = 1

         FOREACH cursor_6a INTO l_record[pos].*

             SELECT desc_status_corta
             INTO   l_record[pos].desc_status_corta
             FROM   pro_status_interno
             WHERE  status_interno = l_record[pos].status_interno

             SELECT A.deleg_desc
             INTO   l_record[pos].delegdesc
             FROM   tab_delegacion A
             WHERE  A.deleg_cod = l_record[pos].deleg

             SELECT A.ciudad_desc
             INTO   l_record[pos].ciudaddesc
             FROM   tab_ciudad A
             WHERE  A.ciudad_cod = l_record[pos].ciudad

             SELECT A.estad_desc
             INTO   l_record[pos].estadodesc
             FROM   tab_estado A
             WHERE  A.estad_cod = l_record[pos].estado                       

             IF pos = 200 THEN
                 LET pos = pos + 1
                 ERROR "  EL ARREGLO HA SIDO SOBREPASADO" ATTRIBUTE(NORMAL)
                 EXIT FOREACH
             END IF

             LET pos = pos + 1
         END FOREACH

         LET cont_reg = pos-1  
                                                                         
         DISPLAY "REGISTROS CARGADOS ",cont_reg ," " AT 21,50 
         ATTRIBUTE(REVERSE) 

         IF (pos-1) >= 1 THEN
             CALL SET_COUNT(pos-1)
             DISPLAY ARRAY l_record TO scr_1.*
                 ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()

                     SELECT * INTO reg_mae.* FROM pro_mae_promotor
                     WHERE  cod_promotor = l_record[pos].cod_promotor

                     LET reg.nro_solicitud  = l_record[pos].nro_solicitud
                     LET reg2.nro_solicitud = l_record[pos].nro_solicitud
                     LET reg.codven         = l_record[pos].codven
                     LET reg2.codven        = l_record[pos].codven
                     LET vcodven            = l_record[pos].codven
                     LET reg.seguro         = l_record[pos].seguro
                     LET reg2.seguro        = l_record[pos].seguro
                     LET reg.rfc            = l_record[pos].rfc
                     LET reg2.rfc           = l_record[pos].rfc
                     LET reg.unico          = l_record[pos].unico
                     LET reg2.unico         = l_record[pos].unico
                     LET reg.paterno        = l_record[pos].paterno
                     LET reg2.paterno       = l_record[pos].paterno
                     LET reg.materno        = l_record[pos].materno   
                     LET reg2.materno       = l_record[pos].materno   
                     LET reg.nombres        = l_record[pos].nombres
                     LET reg2.nombres       = l_record[pos].nombres
                     LET reg.diag_proceso   = l_record[pos].diag_proceso
                     LET reg2.diag_proceso  = l_record[pos].diag_proceso
                     LET reg.fnaci          = l_record[pos].fnaci
                     LET reg2.fnaci         = l_record[pos].fnaci
                     LET reg.fecha_baja     = l_record[pos].fecha_baja
                     LET reg2.fecha_baja    = l_record[pos].fecha_baja
                     LET reg.fingre         = l_record[pos].fingre
                     LET reg2.fingre        = l_record[pos].fingre
                     LET reg.fenvio         = l_record[pos].fenvio
                     LET reg2.fenvio        = l_record[pos].fenvio
                     LET reg.fecha_registro = l_record[pos].fecha_registro
                     LET reg2.fecha_registro= l_record[pos].fecha_registro
                     LET reg.fecha_proceso  = l_record[pos].fecha_proceso
                     LET reg2.fecha_proceso = l_record[pos].fecha_proceso
                     LET reg.resuelva       = l_record[pos].resuelva
                     LET reg2.resuelva      = l_record[pos].resuelva
                     LET reg2.horas_capacit = l_record[pos].horas_capacit
                     LET reg.escolar        = l_record[pos].escolar  
                     LET reg2.escolar       = l_record[pos].escolar  
                     LET reg.calle          = l_record[pos].calle
                     LET reg2.calle         = l_record[pos].calle
                     LET reg.numero         = l_record[pos].numero
                     LET reg2.numero        = l_record[pos].numero
                     LET reg.dpto           = l_record[pos].dpto
                     LET reg2.dpto          = l_record[pos].dpto
                     LET reg.codpos         = l_record[pos].codpos
                     LET reg2.codpos        = l_record[pos].codpos
                     LET reg.colonia        = l_record[pos].colonia
                     LET reg2.colonia       = l_record[pos].colonia
                     LET reg.deleg          = l_record[pos].deleg
                     LET reg2.deleg         = l_record[pos].deleg
                     LET reg.delegdesc      = l_record[pos].delegdesc
                     LET reg2.delegdesc     = l_record[pos].delegdesc
                     LET reg.ciudad         = l_record[pos].ciudad
                     LET reg2.ciudad        = l_record[pos].ciudad
                     LET reg.ciudaddesc     = l_record[pos].ciudaddesc
                     LET reg2.ciudaddesc    = l_record[pos].ciudaddesc
                     LET reg.estado         = l_record[pos].estado
                     LET reg2.estado        = l_record[pos].estado
                     LET reg.estadodesc     = l_record[pos].estadodesc
                     LET reg2.estadodesc    = l_record[pos].estadodesc
                     LET reg.fono           = l_record[pos].fono
                     LET reg2.fono          = l_record[pos].fono
                     LET reg.cod_promotor   = l_record[pos].cod_promotor
                     LET reg2.cod_promotor  = l_record[pos].cod_promotor
                     LET salida = "S"
                     EXIT DISPLAY

                 ON KEY (INTERRUPT,CONTROL-C)
                     LET salida = "N"
                     EXIT DISPLAY
             END DISPLAY

             CASE salida
--                 WHEN "S"
--                     RETURN salida
                 WHEN "N"
                     RETURN salida
                 OTHERWISE
                     EXIT CASE
             END CASE
         ELSE
             ERROR "  REGISTROS DE PROMOTORES....NO EXISTE" ATTRIBUTE(NORMAL)
             SLEEP 3
             ERROR ""
             LET salida = "N"
             RETURN salida
         END IF

         SELECT B.folio,
                B.agenc_cod,
                B.status,
                B.status_interno,
                B.num_lote,
                B.nip,
                B.sup,
                B.nivel
         INTO   reg2.folio,
                reg2.agenc_cod,
                reg2.status, 
                reg2.status_interno, 
                reg2.num_lote,  
                reg2.nip,  
                reg2.sup,  
                reg2.nivel 
         FROM   pro_solicitud B
         WHERE  B.cod_promotor = reg.cod_promotor 

         DISPLAY " ESC: Grabar     Ctrl-C: Salir  ","" AT 1,1
         INPUT BY NAME reg.nro_solicitud THRU reg.fono  WITHOUT DEFAULTS
             BEFORE FIELD nro_solicitud
                      NEXT FIELD resuelva
             AFTER FIELD resuelva
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD fingre
                 END IF

                 IF reg.resuelva  IS NULL  OR  reg.resuelva = " " THEN
                     ERROR "  Campo no puede ser nulo " ATTRIBUTE(NORMAL)
                     NEXT FIELD resuelva
                 END IF

                 IF reg.resuelva < 8 OR reg.resuelva > 10 THEN
                    ERROR "  ERROR...CALIFICACION DEBE SER UN ENTERO MAYOR A 7 ", 
                          "  Y MENOR O IGUAL A 10" ATTRIBUTE(NORMAL)
                    NEXT FIELD resuelva
                END IF

             AFTER FIELD horas_capacit
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                   NEXT FIELD resuelva
                END IF

                IF reg.horas_capacit < 0 THEN
                   ERROR "  NUMERO DE HORAS DE CAPACITACION NO PUEDE SER",
                   "  MENOR A 0 "
                   ATTRIBUTE(NORMAL)
                   NEXT FIELD horas_capacit
                ELSE
                    NEXT FIELD escolar
                END IF

             AFTER FIELD escolar
                IF reg.escolar IS NULL THEN
                    CALL grado_escolar()
                    RETURNING reg.escolar

                    IF reg.escolar IS NULL THEN
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD escolar
                    ELSE
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD calle
                    END IF
                ELSE
                    IF reg.escolar MATCHES "[ABCDEFGHIJKL]" THEN
                       NEXT FIELD calle
                    ELSE
                        ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                        NEXT FIELD escolar
                    END IF
                END IF


            ON KEY( ESC )
                IF reg.resuelva IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese"
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD resuelva
                END IF

                IF reg.horas_capacit < 0 THEN
                    ERROR "  NUMERO DE HORAS DE CAPACITACION NO PUEDE",
                          "  SER MENOR A 0 " ATTRIBUTE(NORMAL)
                    NEXT FIELD horas_capacit
                END IF

               WHILE TRUE                   
                   PROMPT "DESEA CONFIRMAR LA REACTIVACION DE ALTA S/N?" 
                          FOR CHAR enter    
                   IF enter MATCHES "[SsNn]" THEN   #1
                      IF enter MATCHES "[Ss]" THEN   #2           
                         LET  HOY2 = CURRENT
                         INSERT INTO pro_his_reactiva 
                                VALUES ( reg2.folio,
                                         reg2.codven,  
                                         reg2.seguro,
                                         reg2.nip,
                                         reg2.agenc_cod,
                                         reg2.unico,
                                         reg2.rfc,
                                         reg2.paterno,
                                         reg2.materno,
                                         reg2.nombres,
                                         reg2.fecha_baja,
                                         reg2.fingre,
                                         reg2.fenvio, 
                                         reg2.calle, 
                                         reg2.numero, 
                                         reg2.dpto, 
                                         reg2.colonia,
                                         reg2.deleg, 
                                         reg2.ciudad, 
                                         reg2.estado, 
                                         reg2.codpos, 
                                         reg2.fono, 
                                         reg2.sup, 
                                         reg2.nivel, 
                                         reg2.resuelva,
                                         reg2.horas_capacit,
                                         reg2.fnaci,           
                                         reg2.diag_proceso,
                                         reg2.fecha_registro,
                                         reg2.status,
                                         reg2.nro_solicitud,
                                         reg2.status_interno,
                                         reg2.fecha_proceso,
                                         reg2.num_lote,
                                         reg2.cod_promotor,
##                                         reg2.tipo_recibo,
                                         0,
                                         reg2.escolar,
                                         HOY2,
                                         USER 
                                       )       

                         INSERT INTO pro_reactiva_mae
                                VALUES (reg_mae.*)
##                                VALUES (reg_mae.*,HOY2)

                         UPDATE pro_solicitud             
                         SET    folio         = 0                     ,
                                codven        = reg_mae.codven        ,
                                paterno       = reg_mae.paterno       ,
                                materno       = reg_mae.materno       ,
                                nombres       = reg_mae.nombres       ,
                                fnaci         = reg_mae.fnaci         ,
                                unico         = reg_mae.unico         ,
                                seguro        = reg_mae.seguro        ,
                                rfc           = reg_mae.rfc           ,
                                calle         = reg_mae.calle         ,
                                numero        = reg_mae.numero        ,
                                dpto          = reg_mae.dpto          ,
                                colonia       = reg_mae.colonia       ,
                                codpos        = reg_mae.codpos        ,
                                deleg         = reg_mae.deleg         ,
                                ciudad        = reg_mae.ciudad        ,
                                estado        = reg_mae.estado        ,
                                fono          = reg_mae.fono          ,
                                resuelva      = reg.resuelva      ,
                                horas_capacit = reg.horas_capacit ,
                                status_interno= 0                 ,  
                                status        = 0                 ,  
                                diag_proceso  = NULL              ,  
                                num_lote      = 0                 ,  
##                                tipo_recibo   = 0                 ,
                                escolar       = reg_mae.escolar  ,
                                fecha_proceso = NULL              ,
                                fecha_registro = NULL             ,
                                fenvio        = NULL
                         WHERE  nro_solicitud = reg.nro_solicitud              

                         ERROR "  REGISTRO REACTIVADO COMO ALTA..." 
                               ATTRIBUTE(NORMAL)
                         SLEEP 2
                         EXIT WHILE
                      ELSE
                         ERROR "  REACTIVACION CANCELADA" ATTRIBUTE(NORMAL)
                         SLEEP 3
                         EXIT WHILE
                      END IF
                   END IF
               END WHILE
               LET salida = "S"
               EXIT INPUT  

            ON KEY (INTERRUPT,CONTROL-C)
                LET salida = "N"
                EXIT INPUT
            END INPUT

            RETURN salida
END FUNCTION  
