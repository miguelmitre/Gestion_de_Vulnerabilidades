#*******************************************************************#
#Proyecto          => Sistema de Afores( MEXICO )                   #
#Propietario       => EFP                                           #
#Programa          => INTB020                                       #
#Descripcion       => GENERA ARCHIVO DE PRECIOS DE ACCION           #
#Sistema           => INT                                           #
#Fecha             => 24-SEPTIEMBRE-2007                            #
#Por               => STEFANIE DANIELA VERA PIÑA                    #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #reg_1
        fecha_notifica  DATE,
        codigo_siefore  LIKE glo_valor_accion.codigo_siefore,
        siefore_desc    CHAR(8),
        precio_del_dia  LIKE glo_valor_accion.precio_del_dia
    END RECORD

    DEFINE #glo #char
        borra_lineas    CHAR(100),
        COMANDO         CHAR(400),
        ch              CHAR(110),
        enter           CHAR(01),
        G_LISTA         CHAR(300),
        G_LISTA_1       CHAR(100),
        G_LISTA_2       CHAR(100),
        c5nom_plano     CHAR(5)  ,
        c11nom_plano    CHAR(11)

    DEFINE #glo #date
        HOY             ,
        fecha_notifica  ,
        vfecha_captura  DATE
 
    DEFINE
        vruta_envio     LIKE seg_modulo.ruta_envio, 
        vcod_afore      LIKE tab_afore_local.codigo_afore
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
	CALL STARTLOG ("INTB020.log")

    CALL inicializa() 
    OPEN WINDOW intb0201  AT 4,4 WITH FORM "INTB0201" ATTRIBUTE(BORDER)
    DISPLAY " INTB020          GENERA ARCHIVO DE PRECIOS DE ACCION                           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME fecha_notifica

    INPUT BY NAME fecha_notifica WITHOUT DEFAULTS
        AFTER FIELD fecha_notifica
            IF fecha_notifica IS NULL THEN
                ERROR "    LA FECHA NO PUEDE SER NULA" ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_notifica
            ELSE
                SELECT "OK"
                FROM   glo_valor_accion 
                WHERE  fecha_valuacion = fecha_notifica
                GROUP BY 1
         
                IF STATUS = NOTFOUND THEN
                    ERROR "    NO EXISTEN PRECIOS DE ACCION" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_notifica
                END IF
            END IF

        ON KEY (ESC)
            IF fecha_notifica IS NULL THEN
                ERROR "    LA FECHA NO PUEDE SER NULA" ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_notifica
            ELSE
                SELECT "OK"
                FROM   glo_valor_accion
                WHERE  fecha_valuacion = fecha_notifica
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    NO EXISTEN PRECIOS DE ACCION" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_notifica
                END IF
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                LET c11nom_plano   = fecha_notifica USING "YYYYMMDD",".PA"
                EXIT WHILE
            ELSE
                PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
           END IF
        END IF
    END WHILE

    CALL primer_paso()

    DISPLAY "ARCHIVO GENERADO EN LA RUTA: "  AT  8,19
    DISPLAY vruta_envio AT 10,19

    DISPLAY "NOMBRE DEL ARCHIVO PLANO     " AT 12,19
    DISPLAY c11nom_plano                       AT 14,19

    LET COMANDO = "chmod 777 ",vruta_envio CLIPPED,"/",c11nom_plano 
    RUN COMANDO

    PROMPT "    PROCESO FINALIZADO ... <ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW intb0201  
END MAIN


FUNCTION inicializa()
#--------------------
    LET HOY = TODAY
    LET fecha_notifica = HOY

    SELECT codigo_afore 
    INTO   vcod_afore    
    FROM   tab_afore_local

    SELECT ruta_envio  
    INTO   vruta_envio  
    FROM   seg_modulo
    WHERE  modulo_cod = "int"

    LET c5nom_plano ="DETPA"
    LET G_LISTA = vruta_envio

END FUNCTION


FUNCTION primer_paso() 
#---------------------

    DECLARE cur_1 CURSOR FOR
    SELECT  A.codigo_siefore,
            A.precio_del_dia,
            B.siefore_desc
    FROM    glo_valor_accion A, tab_siefore B
    WHERE   A.fecha_valuacion = fecha_notifica
    AND     A.codigo_siefore IN (SELECT siefore_cod
                                 FROM   tab_siefore
                                 WHERE  afore_cod = vcod_afore)
    AND     A.codigo_siefore = B.siefore_cod
    AND     B.afore_cod = vcod_afore

    LET G_LISTA_1 = G_LISTA CLIPPED,"/",c5nom_plano
                                             
    START REPORT listado_1 TO G_LISTA_1

    FOREACH cur_1 INTO reg_1.codigo_siefore,
                       reg_1.precio_del_dia

        SELECT siefore_desc
        INTO   reg_1.siefore_desc
        FROM   tab_siefore
        WHERE  afore_cod = vcod_afore
        AND    siefore_cod = reg_1.codigo_siefore

        CALL habil_anterior(fecha_notifica,2) RETURNING vfecha_captura

        OUTPUT TO REPORT listado_1(reg_1.*) 

    END FOREACH

    FINISH REPORT listado_1

    LET G_LISTA_2 = G_LISTA CLIPPED,"/",c11nom_plano
    LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",G_LISTA_2 CLIPPED
    run borra_lineas

    LET ch = "cp ",G_LISTA_2 CLIPPED," ",G_LISTA_1 CLIPPED
    RUN ch

END FUNCTION


REPORT listado_1(reg_2)
#----------------------

    DEFINE reg_2 RECORD #reg_2
        fecha_notifica  DATE,
        codigo_siefore  LIKE glo_valor_accion.codigo_siefore,
        siefore_desc    LIKE tab_siefore.siefore_desc,
        precio_del_dia  LIKE glo_valor_accion.precio_del_dia
    END RECORD

    DEFINE #loc #char
        c15_precio_dia  CHAR(15),
        c16_precio_dia  CHAR(16)

    DEFINE #loc #smallint
        cont            SMALLINT


    OUTPUT
    PAGE LENGTH    1000
    LEFT MARGIN    0
    RIGHT MARGIN   0
    TOP MARGIN     0
    BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER           
        PRINT
            COLUMN 001,"01"                           ,--tipo de registro
            COLUMN 003,"01"                           ,--identificador de operacion
            COLUMN 005,"01"                           ,--tipo entidad origen
            COLUMN 007,vcod_afore USING"&&&"          ,--clave entidad origen
            COLUMN 010,"03"                           ,--tipo entidad destino
            COLUMN 012,vfecha_captura USING "YYYYMMDD",--fecha notifica  
            COLUMN 020,"         "                    ,--motivo rechazo lote
            COLUMN 029,72 SPACES

    ON EVERY ROW
        LET cont = cont + 1

        LET c16_precio_dia = reg_1.precio_del_dia USING"&&&&&&&&&.&&&&&&"               
        LET c15_precio_dia = c16_precio_dia[01,09],
                             c16_precio_dia[11,16]

        PRINT
            COLUMN 001,"02"                                ,--tipo de registro
            COLUMN 003,"01"                                ,--tipo entidad      
            COLUMN 005,vcod_afore USING"&&&"               ,--clave entidad    
            COLUMN 008,reg_1.siefore_desc                  ,--clave siefore
            COLUMN 016,fecha_notifica USING "YYYYMMDD"     ,--fecha valor
            COLUMN 024,c15_precio_dia                      ,--precio accion     
            COLUMN 039,62 SPACES
            
    ON LAST ROW
        PRINT
            COLUMN 001,"09"                      ,--tipo de registro
            COLUMN 003,"01"                      ,--tipo entidad              
            COLUMN 005,vcod_afore USING"&&&"     ,--clave entidad            
            COLUMN 008,cont USING"&&"            ,--num reg detalle     
            COLUMN 010,91 SPACES
END REPORT


FUNCTION habil_anterior(diaActual,num_dia)
#ha---------------------------------------
    DEFINE #smallint
        cont_1                 ,
        feriado                ,
        finSemana              ,
        diaSemana              ,
        contador               ,
        num_dia                SMALLINT

    DEFINE #date
        diaActual              ,
        diaHabilAnt            ,
        diaTmp                 DATE

    LET cont_1      = 0
    LET diaHabilAnt = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilAnt)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        ELSE
            SELECT *
            FROM   tab_feriado
            WHERE  feria_fecha = diaHabilAnt

            IF STATUS <> NOTFOUND THEN
                LET feriado = 1
            END IF
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
            LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
        ELSE
            LET cont_1      = cont_1 + 1

            IF cont_1 = num_dia THEN
                EXIT WHILE
            ELSE
                LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
            END IF
        END IF
    END WHILE

    RETURN diaHabilAnt
END FUNCTION
