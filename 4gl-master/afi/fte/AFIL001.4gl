#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.        					    #
#Programa AFIL001  => LISTADO DE AFILIADOS PARA ENVIO A CERITIFCAR          # 
#Sistema           => AFI. 					            #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 17 DE ENERO DE 2001                                   #
#############################################################################
DATABASE safre_af
GLOBALS
    DEFINE w_aux  RECORD LIKE afi_solicitud.*

    DEFINE   		
        cont       INTEGER,
        HOY        DATE,
        enter      CHAR(1),
        opcion     CHAR(1),
        HORA       CHAR(8),
        HORA1      CHAR(4),
	g_usuario  CHAR(8),
	G_LISTA    CHAR(100),
	G_CHMOD    CHAR(100),
        g_imprime  CHAR(100),
        g_eco      CHAR(100),
        nom_arch   CHAR(100)

	DEFINE g_afore       RECORD LIKE tab_afore_local.*
	DEFINE g_paramgrales RECORD LIKE seg_modulo.*

END GLOBALS

MAIN 
    OPTIONS PROMPT LINE LAST-1,
    INPUT WRAP                ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT
 
    CALL inicio()
    CALL proceso_principal() #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 02,2 WITH FORM "AFIL0011" ATTRIBUTE(BORDER)
    DISPLAY "AFIL001             LISTADO AFILIADOS PARA CERTIFICACION                       " AT 3,1 ATTRIBUTE (REVERSE)

    DISPLAY hoy USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME opcion WITHOUT DEFAULTS
        AFTER FIELD opcion
            IF opcion IS NULL THEN
                ERROR "Campo no puede ser nulo"
                NEXT FIELD opcion
            END IF
            
            WHILE TRUE
                IF opcion NOT MATCHES "[12]" THEN --"[SsNn]" THEN
                    LET opcion = ""
                    NEXT FIELD opcion
                ELSE
                    IF opcion <> "1" AND opcion <> "2" THEN  ---= "N" THEN
                        EXIT PROGRAM
                    ELSE
                        EXIT WHILE
                    END IF
                END IF
            END WHILE
            EXIT INPUT
    END INPUT 

    DECLARE cur_1 CURSOR FOR
    SELECT *
    FROM   afi_solicitud
    WHERE  status_interno = 20
    AND    tipo_solicitud = opcion
    ORDER BY usuario, tipo_solicitud, n_folio, tipo_solicitud 

    LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,".SOLIC_CERT." CLIPPED,
                  HOY USING "ddmmyy","_",HORA1 CLIPPED

    START REPORT listado TO G_LISTA
        FOREACH cur_1 INTO w_aux.*
            OUTPUT TO REPORT listado (w_aux.*)
        END FOREACH
    FINISH REPORT listado

    LET G_CHMOD = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,".SOLIC_CERT." CLIPPED,
                  HOY USING "ddmmyy","_",HORA1 CLIPPED

    LET nom_arch = g_usuario CLIPPED,".SOLIC_CERT." CLIPPED,
                   HOY USING "ddmmyy","_",HORA1 CLIPPED
    RUN G_CHMOD

    LET g_eco = "echo ", nom_arch CLIPPED, " > ", 
                g_paramgrales.ruta_listados CLIPPED,"/rescate.SOLIC_CERT.",
                HOY USING "ddmmyy","_",HORA1 CLIPPED

    RUN g_eco

    LET g_imprime = "lp ",g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".SOLIC_CERT." CLIPPED,
                    HOY USING "ddmmyy","_",HORA1 CLIPPED

    RUN g_imprime

    EXIT PROGRAM

END FUNCTION

FUNCTION inicio()
#i---------------

    LET cont   = 0
    --LET opcion = 1
    LET HOY    = TODAY
    LET HORA   = TIME
    LET HORA1  = HORA[1,2],HORA[4,5]

    SELECT *,USER 
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore_local

    SELECT * 
    INTO   g_paramgrales.* 
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

END FUNCTION

REPORT listado(w_aux)
#-------------------

    DEFINE w_aux RECORD LIKE afi_solicitud.*

    DEFINE 
        razon_social CHAR(40) 

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH   89

    FORMAT
    PAGE HEADER
        SELECT A.razon_social
        INTO   razon_social
        FROM   tab_afore_local A

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

        PRINT
            COLUMN 01,"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"

        PRINT
            COLUMN   1,"CONS."         ,
            COLUMN   8,"PROMOTOR"      ,
            COLUMN  20,"F.REC"         ,
            COLUMN  31,"NSS"           ,
            COLUMN  43,"CURP"          ,
            COLUMN  63,"RFC"           ,
            COLUMN  78,"NOMBRE"        ,
            COLUMN 118,"FOLIO SOLIC."  ,
            COLUMN 133,"F.DOC.PROB."   ,
            COLUMN 143,"DOC.PROB."     ,
            COLUMN 163,"USUARIO"       ,
            COLUMN 172,"FOL EDO CTA"   ,
            COLUMN 185,"F.EMISION"     ,
            COLUMN 196,"AFORE CED"
        PRINT
            COLUMN 01,"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"

    ON EVERY ROW
        LET cont = cont + 1
        PRINT
            COLUMN   1,cont                    USING "#####"    ,
            COLUMN   8,w_aux.codven                               ,
            COLUMN  20,w_aux.frecafor          USING "DD-MM-YYYY" ,
            COLUMN  31,w_aux.n_seguro                             ,
            COLUMN  43,w_aux.n_unico                              ,
            COLUMN  63,w_aux.n_rfc                                ,
            COLUMN  78,w_aux.paterno           CLIPPED ," ",
                       w_aux.materno           CLIPPED ," ",
                       w_aux.nombres           CLIPPED ," ",  
            COLUMN 118,w_aux.tipo_solicitud    USING "#"          ,
            COLUMN 120,w_aux.n_folio                             ,
            COLUMN 133,w_aux.fol_prob                            ,
            COLUMN 143,w_aux.doc_prob                            ,
            COLUMN 163,w_aux.usuario       ,
            COLUMN 172,w_aux.folio_edo_cta ,
            COLUMN 185,w_aux.femision      ,
            COLUMN 196,w_aux.cod_afore_ced
        
END REPORT

