###############################################################################
#Programa PROM005  => MANTENEDOR DE HORAS DE CAPACITACION Y CALIFICACION DEL  #
#Sistema           => PRO. 					                                          #
#                  => AGENTE PROMOTOR                                         #
#Fecha             => 10 DE ENERO DE 2000				                              #
#ELABORADO POR     => FRANCO ESTEBAN ULLOA VIDELA  	 		                      #
#Fecha actualiz.   => 01 DE ABRIL DEL 2004                                    #
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN                             #
#CUO               => FSR 03/08/2015 ACTUALIZACION DEL NUEVO LAYOUT           #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        cod_promotor          LIKE pro_mae_promotor.cod_promotor ,
        nombre                CHAR(30)             ,
        calificacion          SMALLINT             ,
        horas                 SMALLINT             ,
        fecha_reval           DATE                 , #CUO #7	Fecha de revalidación del registro
        diag_reval            CHAR(02)             , #CUO #8	Diagnóstico del proceso de la solicitud de revalidación
        fecha_vence           DATE                 , #CUO #10	Fecha de vencimiento de Revalidación
        sub_mtvo              CHAR(02)
    END RECORD

    DEFINE reg_3 RECORD #glo #reg_3 #SOLO PARA AGREGA
        cod_promotor          LIKE pro_mae_promotor.cod_promotor ,
        nombre                CHAR(30)             ,
        calificacion          SMALLINT             ,
        horas                 SMALLINT             
    END RECORD   
    
    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(1),
        vestado_desc          CHAR(20) 

    DEFINE #glo #smallint
        vestado            SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV("USER")||".PROM005.log")
    CALL ERRORLOG ("Versión :CUO")
    
    CALL init() #i
    OPEN WINDOW prom0051 AT 4,4 WITH FORM "PROM0051" ATTRIBUTE(BORDER)
    DISPLAY " PROM005           MANTENIMIENTO DE CALIFICA",
            "CIONES                                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY " <ESC> Aceptar                              ",
            "                <Ctrl-C> Salir    " AT 5,1 ATTRIBUTE(REVERSE)

    MENU "MENU"
        COMMAND KEY(A) "Agrega" "Agrega calificacion y horas de capacion"
            CALL inicializa() #i
            CALL agrega()     #a
            CLEAR FORM

        COMMAND KEY(C) "Consulta" "Consulta calificacion y horas de capacion"
            CALL inicializa() #i
            CALL consulta()   #c
            CLEAR FORM

        COMMAND KEY(M) "Modifica" "Modifica calificacion y horas de capacion"
            CALL inicializa() #i
            CALL modifica()   #m
            CLEAR FORM

        COMMAND KEY(N) "eNvio" "Consulta de Registros para Enviar Revalidacion"
            CALL inicializa() #i
            CALL con_envio()    #e
            CLEAR FORM

        COMMAND KEY(E) "Elimina" "Elimina calificacion y horas de capacion"
            CALL inicializa() #i
            CALL elimina()    #e
            CLEAR FORM

        COMMAND KEY("O") "repOrte Env" "Genera Reporte Envio Asesor"
            CALL genera_rep_asesor()
            CLEAR FORM           

        COMMAND KEY(S) "Salir" "Salir del programa"
            EXIT PROGRAM
    END MENU
    CLOSE WINDOW prom0051
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
END FUNCTION 

FUNCTION inicializa()
#i-------------------
    INITIALIZE reg_1.* TO NULL
    INITIALIZE reg_3.* TO NULL
END FUNCTION

FUNCTION agrega()
#a---------------
    DEFINE reg_2 RECORD #loc #reg_2
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        nombres               CHAR(40)
    END RECORD

    DEFINE #loc #date
        f_fecha_registro      DATE

    DEFINE #loc #smallint
        s_status              SMALLINT, 
        lc_indica_vig         CHAR(01)


    OPEN WINDOW prom0053 AT 4,4 WITH FORM "PROM0053" ATTRIBUTE(BORDER)
    DISPLAY " PROM005           MANTENIMIENTO DE CALIFICA",
            "CIONES                                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY " <ESC> Aceptar                              ",
            "                <Ctrl-C> Salir    " AT 5,1 ATTRIBUTE(REVERSE)        
    LET lc_indica_vig = " "
    
    INPUT BY NAME reg_3.* WITHOUT DEFAULTS
        AFTER FIELD cod_promotor
            IF reg_3.cod_promotor IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO" 
                NEXT FIELD cod_promotor
            END IF

            SELECT UNIQUE "OK"
            FROM   pro_capacitacion
            WHERE  cod_promotor = reg_3.cod_promotor
            #AND    estado       = 1
            GROUP BY 1

            IF SQLCA.SQLCODE = 0  THEN
                ERROR "REGISTRO YA INGRESADO, MODIFICAR POR MEDIO DE LA OPCION 'MODIFICA'" 
                NEXT FIELD cod_promotor
            END IF
 

            SELECT A.fecha_registro ,
                   A.status         ,
                   A.paterno        ,
                   A.materno        ,
                   A.nombres
            INTO   f_fecha_registro ,
                   s_status         ,
                   reg_2.paterno    ,
                   reg_2.materno    ,
                   reg_2.nombres
            FROM   pro_mae_promotor A
            WHERE  A.cod_promotor = reg_3.cod_promotor

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "CODIGO DE PROMOTOR INEXISTENTE" 
                NEXT FIELD cod_promotor
            ELSE
                IF s_status <> 1 AND s_status <> 4 THEN
                    ERROR "PROMOTOR NO ACTIVO" 
                    NEXT FIELD cod_promotor
                ELSE
                    LET f_fecha_registro = f_fecha_registro + 2 UNITS YEAR
                END IF
            END IF

            LET reg_3.nombre = reg_2.paterno CLIPPED," ",
                               reg_2.materno CLIPPED," ",
                               reg_2.nombres CLIPPED
            DISPLAY BY NAME reg_3.nombre

        AFTER FIELD calificacion 
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD cod_promotor
            END IF

            IF reg_3.calificacion < 75 OR reg_3.calificacion > 100 
            	THEN
            	  #SLEEP 3
                ERROR "ERROR..CALIFICACION DEBE SER UN ENTERO >= 75% ",
                      "ó <= 100" ATTRIBUTE(NORMAL)
                NEXT FIELD calificacion
            END IF

        #AFTER FIELD horas 
        #    IF reg_3.horas < 60 THEN
        #        ERROR "ERROR...EL MINIMO DE HORAS ES 60" 
        #        NEXT FIELD horas
        #    END IF

        ON KEY (ESC)
            SELECT UNIQUE "OK"
            FROM   pro_capacitacion
            WHERE  cod_promotor = reg_3.cod_promotor
            AND    estado       = 1

            IF SQLCA.SQLCODE = 0  THEN
                ERROR "REGISTRO YA INGRESADO" 
                NEXT FIELD cod_promotor
            END IF
 
            IF reg_3.cod_promotor IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD cod_promotor
            END IF
        
            IF reg_3.calificacion IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD calificacion
            END IF
        
            IF reg_3.horas IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD horas
            END IF
                                   
            INSERT INTO pro_capacitacion VALUES(reg_3.cod_promotor       ,
                                        reg_3.calificacion ,
                                        reg_3.horas        ,
                                        1                  ,#estado
                                        HOY                , 
                                        lc_indica_vig      ) #Indicador de vigencia CUO, la primera vez se deberá enviar un espacio
                                                           
            ERROR "REGISTRO INGRESADO" 
            SLEEP 3
            CLEAR FORM
            CALL inicializa() #i
            EXIT INPUT
       
        ON KEY (CONTROL-C,INTERRUPT)
            EXIT INPUT
    END INPUT
    DISPLAY "" AT 18,1
    ERROR ""
   CLOSE WINDOW prom0053
END FUNCTION

FUNCTION consulta()
#c-----------------
    DEFINE reg_2 RECORD #loc #reg_2
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        nombres               CHAR(40)
    END RECORD

    DEFINE reg_3 RECORD #loc #reg_3
        calificacion          SMALLINT ,
        horas                 SMALLINT
    END RECORD

    INPUT BY NAME reg_1.cod_promotor WITHOUT DEFAULTS
        BEFORE FIELD cod_promotor
            LET reg_1.calificacion = NULL
            LET reg_1.horas        = NULL

        AFTER FIELD cod_promotor 
            SELECT UNIQUE "a.X" 
            FROM pro_capacitacion a
            WHERE  a.cod_promotor = reg_1.cod_promotor
            #AND    a.estado       = 1 #SE QUITA VALIDACION PARA CONSULTAR EN CUALQUIER ESTADO
            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "PROMOTOR NO EXISTE"
               NEXT FIELD cod_promotor
            END IF

            SELECT paterno       ,
                   materno       ,
                   nombres
            INTO   reg_2.paterno ,
                   reg_2.materno ,
                   reg_2.nombres
            FROM   pro_mae_promotor
            WHERE  cod_promotor = reg_1.cod_promotor
            GROUP BY 1,2,3

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "PROMOTOR NO EXISTE"
                LET reg_1.nombre       = NULL 
                LET reg_1.calificacion = NULL
                LET reg_1.horas        = NULL
                DISPLAY BY NAME reg_1.*
                NEXT FIELD cod_promotor
            END IF

            SELECT calificacion       ,
                   horas              ,
                   estado
            INTO   reg_1.calificacion ,
                   reg_1.horas        ,
                   vestado
            FROM   pro_capacitacion
            WHERE  cod_promotor = reg_1.cod_promotor
            #AND    estado       = 1
            
            
            SELECT fecha_reval, 
                   diag_reval ,
                   fecha_vence,
                   sub_mtvo    #CPL-2295
            INTO   reg_1.fecha_reval, 
                   reg_1.diag_reval ,
                   reg_1.fecha_vence,
                   reg_1.sub_mtvo #CPL-2295
            FROM pro_det_revalida
            WHERE cod_promotor = reg_1.cod_promotor
            AND folio IN (       
                         SELECT MAX(folio)
                         FROM pro_det_revalida
                         WHERE cod_promotor = reg_1.cod_promotor)

            LET reg_1.nombre = reg_2.paterno CLIPPED," ",
                               reg_2.materno CLIPPED," ",
                               reg_2.nombres CLIPPED

            CASE vestado
                WHEN 1
                   LET vestado_desc = "CAPTURADO "
                WHEN 2
                   LET vestado_desc = "DETALLE GENERADO "
                WHEN 3
                   LET vestado_desc = "LOTE GENERADO "
                WHEN 4 
                	 LET vestado_desc = "RECHAZADO "
                WHEN 5 
                	 LET vestado_desc = "ACEPTADO "	                    
            END CASE
    
            DISPLAY BY NAME reg_1.*

            DISPLAY " ",vestado_desc CLIPPED AT 18,1 ATTRIBUTE(REVERSE)

        ON KEY (INTERRUPT,CONTROL-C)
            DISPLAY "                         " AT 18,1 
            EXIT INPUT
    END INPUT
    DISPLAY "" AT 18,1 
    ERROR ""
END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE reg_2 RECORD #loc #reg_2
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        nombres               CHAR(40)
    END RECORD

    INPUT BY NAME reg_1.* WITHOUT DEFAULTS
        AFTER FIELD cod_promotor
            SELECT "a.X" 
            FROM pro_capacitacion a
            WHERE  a.cod_promotor = reg_1.cod_promotor
            #AND    a.estado       = 1
            
            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "PROMOTOR NO EXISTE, FAVOR AGREGAR"
               NEXT FIELD cod_promotor
            END IF

            SELECT paterno       ,
                   materno       ,
                   nombres
            INTO   reg_2.paterno ,
                   reg_2.materno ,
                   reg_2.nombres
            FROM   pro_mae_promotor
            WHERE  cod_promotor = reg_1.cod_promotor

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "PROMOTOR NO EXISTE"
                NEXT FIELD cod_promotor
            END IF

            SELECT calificacion       ,
                   horas              ,
                   estado 
            INTO   reg_1.calificacion ,
                   reg_1.horas        ,
                   vestado    
            FROM   pro_capacitacion
            WHERE  cod_promotor = reg_1.cod_promotor
            
            SELECT fecha_reval, 
                   diag_reval ,
                   fecha_vence, 
                   sub_mtvo
            INTO   reg_1.fecha_reval, 
                   reg_1.diag_reval ,
                   reg_1.fecha_vence, 
                   reg_1.sub_mtvo #CPL-2295
            FROM pro_det_revalida
            WHERE cod_promotor = reg_1.cod_promotor
            AND folio IN (       
                         SELECT MAX(folio)
                         FROM pro_det_revalida
                         WHERE cod_promotor = reg_1.cod_promotor)

            LET reg_1.nombre = reg_2.paterno CLIPPED," ",
                               reg_2.materno CLIPPED," ",
                               reg_2.nombres CLIPPED
            
            
            DISPLAY BY NAME reg_1.*
            
            CASE vestado
                WHEN 1
                   LET vestado_desc = "CAPTURADO "
                WHEN 2
                   PROMPT "DETALLE GENERADO NO SE PUEDE MOFIDICAR, <ENTER> PARA SALIR " FOR CHAR enter
                   EXIT PROGRAM
                WHEN 3
                   PROMPT "LOTE GENERADO NO SE PUEDE MOFIDICAR, <ENTER> PARA SALIR " FOR CHAR enter
                   EXIT PROGRAM
                WHEN 4 
                   LET vestado_desc = "RECHAZADO "
                WHEN 5 
                   LET vestado_desc = "ACEPTADO "
            END CASE                                          

            DISPLAY " ",vestado_desc CLIPPED AT 18,1 ATTRIBUTE(REVERSE)     

        AFTER FIELD calificacion 
            IF reg_1.calificacion < 75   OR 
               reg_1.calificacion > 100 THEN
               ERROR "Digite correctamente la calificacion, debe ser mayor o igual a 75%"
               NEXT FIELD calificacion
            END IF

        AFTER FIELD horas 
            IF reg_1.horas < 0  OR
               reg_1.horas > 999 THEN
               ERROR "Digite correctamente las horas de capacitacion"
               NEXT FIELD horas
            END IF

        ON KEY (ESC)
            IF reg_1.calificacion IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD calificacion
            END IF

            IF reg_1.calificacion < 75   OR 
               reg_1.calificacion > 100 THEN
               ERROR "Digite correctamente la calificacion, debe ser mayor o igual a 75%"
               NEXT FIELD calificacion
            END IF
        
            IF reg_1.horas IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD horas
            END IF
            IF reg_1.horas < 0  OR
               reg_1.horas > 999 THEN
               ERROR "Digite correctamente las horas de capacitacion"
               NEXT FIELD horas
            END IF
            
            INSERT INTO pro_his_capacita
            SELECT *, TODAY
            FROM pro_capacitacion
            WHERE  pro_capacitacion.cod_promotor = reg_1.cod_promotor
            
            UPDATE pro_capacitacion
            SET    pro_capacitacion.calificacion = reg_1.calificacion ,
                   pro_capacitacion.horas        = reg_1.horas        ,
                   pro_capacitacion.estado       = 1                  ,# PARA VOLVER A ENVIARLO
                   pro_capacitacion.fecha_genera = NULL               ,
                   pro_capacitacion.ind_vigencia = "1" #NO ES POR PRIMERA VEZ
            WHERE  pro_capacitacion.cod_promotor = reg_1.cod_promotor
            #AND    pro_capacitacion.estado       = 1

            DISPLAY " Registro Modificado" AT 18,1
            SLEEP 3
            CALL inicializa() #i
            CLEAR FORM
            EXIT INPUT

        ON KEY (INTERRUPT,CONTROL-C)
            DISPLAY "                         " AT 18,1      
            EXIT INPUT
    END INPUT
    DISPLAY "" AT 18,1
    ERROR ""
END FUNCTION

FUNCTION elimina()
#e----------------
    DEFINE reg_2 RECORD #loc #reg_2
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        nombres               CHAR(40)
    END RECORD

    INPUT BY NAME reg_1.cod_promotor WITHOUT DEFAULTS
        AFTER FIELD cod_promotor
            SELECT "a.X" 
            FROM pro_capacitacion a
            WHERE  a.cod_promotor = reg_1.cod_promotor
            #AND    a.estado       = 1
            GROUP BY 1
            
            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "PROMOTOR NO EXISTE"
               NEXT FIELD cod_promotor
            END IF

            SELECT paterno       ,
                   materno       ,
                   nombres
            INTO   reg_2.paterno ,
                   reg_2.materno ,
                   reg_2.nombres
            FROM   pro_mae_promotor
            WHERE  cod_promotor = reg_1.cod_promotor

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "PROMOTOR NO EXISTE"
                NEXT FIELD cod_promotor
            END IF

            SELECT calificacion       ,
                   horas              ,
                   estado
            INTO   reg_1.calificacion ,
                   reg_1.horas        , 
                   vestado
            FROM   pro_capacitacion
            WHERE  cod_promotor = reg_1.cod_promotor

            LET reg_1.nombre = reg_2.paterno CLIPPED," ",
                               reg_2.materno CLIPPED," ",
                               reg_2.nombres CLIPPED

            CASE vestado
                WHEN 1
                   LET vestado_desc = "CAPTURADO "
                WHEN 2
                   PROMPT "DETALLE GENERADO NO SE PUEDE ELIMINAR, <ENTER> PARA SALIR " FOR CHAR enter
                   EXIT PROGRAM
                WHEN 3
                   PROMPT "LOTE GENERADO NO SE PUEDE ELIMINAR, <ENTER> PARA SALIR " FOR CHAR enter
                   EXIT PROGRAM
                WHEN 4 
                	 LET vestado_desc = "RECHAZADO "
                WHEN 5 
                	 LET vestado_desc = "ACEPTADO "	                    
            END CASE                                             

            DISPLAY BY NAME reg_1.*

            DISPLAY " ",vestado_desc CLIPPED AT 18,1 ATTRIBUTE(REVERSE)      

        ON KEY (ESC)

           WHILE TRUE
               PROMPT "ESTA SEGURO S/N ? " FOR enter
               IF enter MATCHES "[sSnN]" THEN
                   IF enter MATCHES "[sS]" THEN
                       EXIT WHILE
                   ELSE
                       DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                       EXIT PROGRAM
                   END IF
               END IF
           END WHILE

            INSERT INTO pro_his_capacita
            SELECT *, TODAY
            FROM pro_capacitacion
            WHERE  cod_promotor = reg_1.cod_promotor

            DELETE
            FROM   pro_capacitacion 
            WHERE  cod_promotor = reg_1.cod_promotor
            #AND    estado = 1

            ERROR "REGISTRO ELIMINADO"
            SLEEP 3
            CALL inicializa() #i
            CLEAR FORM
            EXIT INPUT

        ON KEY (INTERRUPT, CONTROL-C)
            DISPLAY "                         " AT 18,1    
            EXIT INPUT
    END INPUT
    ERROR ""
    DISPLAY "" AT 18,1
END FUNCTION

FUNCTION con_envio()
    DEFINE
        x_busca               CHAR(500) ,
        txt_1                 CHAR(1500)

    DEFINE
        arr_c                 ,
        arc                   ,
        sw_1                  ,
        pos                   SMALLINT,
        total                 INTEGER

    DEFINE arr_1 ARRAY[2000] OF RECORD
               cod_promotor       CHAR(10),
               nombre             CHAR(30),
               califica           SMALLINT,
               horas              SMALLINT
           END RECORD,

           arr_reg   RECORD
               cod_promotor       CHAR(10),
               nombre             CHAR(30),
               paterno            CHAR(30),
               materno            CHAR(30),
               califica           SMALLINT,
               horas              SMALLINT
           END RECORD

    INITIALIZE x_busca, txt_1 TO NULL

    LET sw_1     = 0
    LET total    = 0

    DECLARE cur_3 CURSOR FOR 
        SELECT A.cod_promotor    ,
               B.nombres         ,
               B.paterno         ,
               B.materno         ,
               A.calificacion    ,
               A.horas 
        FROM   pro_capacitacion A,pro_mae_promotor B
        WHERE  A.cod_promotor = B.cod_promotor 
        AND    A.estado = 1 

        LET pos = 1

    FOREACH cur_3 INTO arr_reg.*
        IF  pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH
        ELSE
            LET arr_1[pos].cod_promotor = arr_reg.cod_promotor
            LET arr_1[pos].nombre       = arr_reg.nombre  CLIPPED," ",
                                          arr_reg.paterno CLIPPED," ",
                                          arr_reg.materno CLIPPED
            LET arr_1[pos].califica     = arr_reg.califica
            LET arr_1[pos].horas        = arr_reg.horas
            LET pos = pos + 1
        END IF
    END FOREACH

    IF pos = 1 THEN
        ERROR" NO HAY REGISTROS"
        RETURN
    END IF


    CALL SET_COUNT(pos-1)

    LET total = pos - 1

    OPEN WINDOW prom0052 AT 8,5 WITH FORM "PROM0052" ATTRIBUTE(BORDER)
    DISPLAY "         CONSULTA DE REGISTROS PARA ENVIO DE ",
            "REVALIDACIONES               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           <Ctrl-C> Salir          ",
            "                       " AT 2,1 ATTRIBUTE(REVERSE)

    DISPLAY total TO FORMONLY.total

    DISPLAY ARRAY arr_1 TO scr_1.*
         ON KEY(INTERRUPT, CONTROL-C)
                EXIT DISPLAY
    END DISPLAY
    CLOSE WINDOW prom0052
END FUNCTION 
FUNCTION genera_rep_asesor()
    DEFINE  rep_1  RECORD
        cod_promotor   CHAR(10),
        unico          CHAR(18),
				folio          INTEGER, 
				fenvio         DATE, 
				frecepcion     DATE, 
				operacion      CHAR(03), 
				fvigencia      DATE, 
				diagnostico    CHAR(02),
				factualiza     DATE
   END RECORD,

            G_LISTA           CHAR(70),
            seg_mod   RECORD LIKE seg_modulo.*

   INITIALIZE rep_1.*, G_LISTA, seg_mod.* TO NULL
   SELECT * INTO seg_mod.* FROM seg_modulo
   WHERE  modulo_cod = "pro"

   OPEN WINDOW prom0512 AT 5,2 WITH FORM "PROM0512" ATTRIBUTE (BORDER)
   DISPLAY " (Ctrl-C) Salir " AT 1,1
   DISPLAY "                     GENERA REPORTE DE ENV.ASESOR PROVISIONAL ",
            "                              " AT 2,1 ATTRIBUTE(REVERSE)

        WHILE TRUE
            PROMPT " DESEA GENERAR EL REPORTE   S / N  ?" FOR enter
            IF enter MATCHES "[SsNn]" THEN
               EXIT WHILE
            END IF
        END WHILE

        IF enter MATCHES "[Nn]" THEN
           PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR..." FOR enter
           RETURN
        END IF

        ERROR "PROCESANDO INFORMACION..."

        LET G_LISTA = seg_mod.ruta_listados CLIPPED,"/archivo_env_asesor.txt"

        START REPORT listado_2 TO G_LISTA
        DECLARE apt_asesor CURSOR FOR
             SELECT *
             from pro_certificado_prov
             order by 1
        FOREACH apt_asesor INTO rep_1.*
             OUTPUT TO REPORT listado_2(rep_1.*)
        END FOREACH
        FINISH REPORT listado_2

        ERROR ""
        DISPLAY "ARCHIVO GENERADO EN :",G_LISTA CLIPPED  AT 14,1
        PROMPT "PROCESO FINALIZADO...<ENTER> PARA CONTINUAR..." FOR enter
   CLOSE WINDOW prom0512

END FUNCTION

REPORT listado_2(rep_1)
    DEFINE  rep_1  RECORD
        cod_promotor CHAR(10),
        unico        CHAR(18),
				folio        INTEGER, 
				fenvio       DATE, 
				frecepcion   DATE, 
				operacion    CHAR(03), 
				fvigencia    DATE, 
				diagnostico  CHAR(02),
				factualiza   DATE
    END RECORD

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
       PAGE HEADER
            PRINT COLUMN 01,"  COD.PROMOTOR  ",
                            "|",
                            "CURP",            
                            "|",
                            "FOLIO",
                            "|",
                            "                 FECHA ENVIO            ",
                            "|",
                            "                 FECHA RECEPCION        ",
                            "|",
                            "                 OPERACION              ",
                            "|",
                            " FECHA VIGENCIA",
                            " |",
                            "DIAGNOSTICO"

       ON EVERY ROW
          PRINT COLUMN 01, rep_1.cod_promotor    ,
                            "| ",
                           rep_1.unico           ,          
                            "| ",
                           rep_1.folio           ,
                            " |",
                           rep_1.fenvio          ,
                            "|",
                           rep_1.frecepcion          ,
                            "|",
                           rep_1.operacion          ,
                            "|",
                           rep_1.fvigencia        ,
                            "  |",
                           rep_1.diagnostico
END REPORT    


