DATABASE safre_af 
GLOBALS
   DEFINE  enter            CHAR(001)
   DEFINE  folio_ced        INTEGER
   DEFINE  g_fecha_envio    DATE
   DEFINE  lanza_proceso    CHAR(1000) 
   DEFINE  g_hoy            DATE
   DEFINE  paso             CHAR(200)
   DEFINE  ruta_envio       CHAR(100)

END GLOBALS 
MAIN        

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

   LET g_hoy = TODAY 
   SELECT a.ruta_envio 
   INTO   ruta_envio
   FROM   seg_modulo a
   WHERE  modulo_cod ="est"

   CALL proceso_principal()
END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ESTB0701 AT 4,4 WITH FORM "ESTB0701" ATTRIBUTE(BORDER)
    DISPLAY "                               <Ctrl-c> Salir                              " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " ESTB070          ESTADISTICA 19-7 id(1008) PARA CONSAR                        " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY g_hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)


    INPUT BY NAME folio_ced  

       BEFORE FIELD folio_ced
       SELECT max(folio)
       INTO folio_ced
       FROM taa_cd_ctr_folio
       WHERE tipo_traspaso = 1
       AND   estado = 103 

       DISPLAY BY NAME folio_ced  

       AFTER FIELD folio_ced

             SELECT "OK"
               FROM safre_af:taa_cd_ctr_folio
             WHERE folio           = folio_ced
               AND tipo_traspaso   = 1
               AND estado          = 103

             IF (STATUS = NOTFOUND AND 
                  folio_ced <> 0 ) THEN
                ERROR "El folio --NO EXISTE-- verificar..."
                SLEEP 2
                ERROR "                                   "   
                LET folio_ced = NULL
                NEXT FIELD folio_ced 
             ELSE 
                EXIT INPUT
             END IF 

        ON KEY (INTERRUPT)
            EXIT PROGRAM

        ON KEY (ESC)

           IF (folio_ced IS NULL OR 
               folio_ced = 0 ) THEN

              INPUT BY NAME g_fecha_envio WITHOUT DEFAULTS

                 BEFORE FIELD g_fecha_envio 
                    LET g_fecha_envio = NULL

                 DISPLAY BY NAME g_fecha_envio
                 DISPLAY "Fecha Envio            :" AT 15,11     
                 
                 AFTER FIELD g_fecha_envio
                    IF ( g_fecha_envio IS  NULL OR 
                         g_fecha_envio < "01/01/1900" ) THEN
                        ERROR "La Fecha es incorrecta verifique..."
                        SLEEP 3 
                        ERROR "                                   "
                        LET  g_fecha_envio  = NULL
                        NEXT FIELD g_fecha_envio   
                    ELSE
                        EXIT INPUT
                    END IF
 
              END INPUT 
           END IF   
              
          EXIT INPUT    
    END INPUT

       WHILE TRUE
        PROMPT "Desea generar estadistica [S/N] : ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
	       EXIT WHILE
            ELSE
		EXIT PROGRAM
            END IF
       END IF
      END WHILE


    DISPLAY "Ejecutando Proceso por nohup ..." AT 19,1 ATTRIBUTE(REVERSE)
SLEEP 2
    DISPLAY "Saliendo de pantalla         ..." AT 19,1 ATTRIBUTE(REVERSE)
SLEEP 2
LET lanza_proceso = "nohup fglgo ESTB071 ",folio_ced ," ",g_fecha_envio

LET paso = ruta_envio CLIPPED,
           "/"                 ,
           "nohup_1008."       ,
           folio_ced USING"&&&&&&&"

LET lanza_proceso  = lanza_proceso CLIPPED, " 1>",paso CLIPPED," 2>&1 &"
RUN lanza_proceso

END FUNCTION

