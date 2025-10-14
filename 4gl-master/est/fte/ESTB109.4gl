################################################################################
#Owner              => E.F.P.
#Programa ESTB109   => ANEXO 64 ( Este Archivo contiene la Informacion de los 
#                     movimientos diarios que se realizan en los procesos 
#                     Operativos de las Administradoras ).  
#Fecha de Creacion   => 10 de Julio del 2008.
#Ultima Modificacion => 12 de Marzo del 2010.
#By                  => Marco Antonio Gonzalez Rojas
#Sistema             =>
###############################################################################

DATABASE safre_af 

GLOBALS

DEFINE  enter            CHAR(001)
DEFINE  fecha_gen        DATE     
DEFINE  g_fecha_envio    DATE
DEFINE  lanza_proceso    CHAR(1000) 
DEFINE  g_hoy            DATE
DEFINE  paso             CHAR(200)
DEFINE  ruta_envio       CHAR(100)
DEFINE  g_dd             , 
        g_mm             ,
        g_yy             CHAR(04)
DEFINE  g_liq_arch       CHAR(08)

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

    OPEN WINDOW ESTB109  AT 4,4 WITH FORM "ESTB1091" ATTRIBUTE(BORDER)
    DISPLAY "                               <Ctrl-c> Salir                              " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " ESTB109                    ANEXO 64  PARA CONSAR                               " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY g_hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)


    INPUT BY NAME fecha_gen  

       BEFORE FIELD fecha_gen
          
          LET    fecha_gen            =            g_hoy          

       DISPLAY BY NAME fecha_gen 

       AFTER FIELD fecha_gen 

             IF ( fecha_gen IS NULL ) THEN
                ERROR "fecha_gen --NO PUEDE SER NULA-- verificar..."
                SLEEP 4
                ERROR " "   
                LET fecha_gen = NULL
                NEXT FIELD fecha_gen 
             ELSE 
                EXIT INPUT
             END IF 

        ON KEY (INTERRUPT)
            EXIT PROGRAM

        ON KEY (ESC)

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

LET lanza_proceso = "nohup fglgo ESTB110 ","'",fecha_gen,"'"    

LET g_dd       =  DAY(fecha_gen)   USING "&&"
LET g_mm       =  MONTH(fecha_gen) USING "&&"
LET g_yy       =  YEAR(fecha_gen)  USING "&&&&"
LET g_liq_arch =  g_dd CLIPPED,g_mm CLIPPED,g_yy

LET paso = ruta_envio CLIPPED,
           "/"                 ,
           "nohup_1110."       ,
            g_liq_arch USING"&&&&&&&&"


LET lanza_proceso  = lanza_proceso CLIPPED, " 1>",paso CLIPPED," 2>&1 &"
RUN lanza_proceso

END FUNCTION
