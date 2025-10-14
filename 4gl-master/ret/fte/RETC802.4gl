#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC802  => LANZADOR DE LA CONSULTA DE LA CARGA DEL ARCHIVO           #
#                     DE DATAMART IMSS                                          #
#Fecha creacion    => 12 DE MARZO DE 2010                                       #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE
        HOY                     DATE

    DEFINE
        enter                   CHAR(001) ,
        gc_comando              CHAR(100) ,
        gc_param_serv           CHAR(003)

    DEFINE
        ls_salida               SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC802")
    CALL f_abre_ventana()    

    LET HOY             = TODAY
    LET gc_param_serv   = "CON"
    LET ls_salida       = 0

    WHILE TRUE
        LET gc_comando = " fglgo RETC801 ", gc_param_serv CLIPPED
        RUN gc_comando
        
        CLOSE WINDOW main_win
        CALL f_abre_ventana()
        
        WHILE TRUE
            PROMPT " ¿REALIZAR OTRA CONSULTA (S/N)? :" FOR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[nN]" THEN
                    LET ls_salida  = 1
                END IF
                EXIT WHILE
            END IF
        END WHILE -- Prompt de consulta
    
        IF ls_salida THEN
            EXIT WHILE
        END IF
    
    END WHILE -- Ejecucion del programa

    CLOSE WINDOW main_win

END MAIN

FUNCTION f_abre_ventana()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC802            CONSULTA DE RESOLUCIONES DATAMART                          " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

END FUNCTION
