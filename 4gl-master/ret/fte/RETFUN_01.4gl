#################################################################################
#Owner             => E.F.P.                                                    #
#                     LIBRERIA DE FUNCIONES DEL MODULO DE RETIROS               #
#                                                                               #
#RETFUN_01          => Funciones de uso general que no implican consultas a     #
#                      tablas de safre                                          #
#                                                                               #
#Fecha creacion    => 23 DE AGOSTO DE 2012                                      #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#                                                                               #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

{
    -- Javier Gonzalez
    -- f_lib_abre_log(pc_nom_programa) se debe ir eliminando de los programas 
    -- para usar en su lugar la funcion f_lib_crea_log
    --
    
Listado de funciones contenidas en RETFUN_01
    
    -   f_lib_crea_log()
    -   f_lib_pregunta(pc_msg)
    -   f_lib_error_msg(pc_mensaje)
    -   f_lib_notifica_error(pc_msg_error)
    -   f_lib_valida_fechas(pdt_fecha)
    -   f_lib_borra_lineas(pc_ruta, pc_archivo)

}

#---------------------------------------------------------------------------#
# f_lib_abre_log : Abre el archivo de bitacora correspondiente al usuario   #
#                  que ejecuta el programa                                  #
#---------------------------------------------------------------------------#
FUNCTION f_lib_abre_log(pc_nom_programa)

    DEFINE
        pc_nom_programa         VARCHAR(15)

    DEFINE
        lc_nombre_log           VARCHAR(40)

    -- -----------------------------------------------------------------------------

    LET lc_nombre_log  = pc_nom_programa CLIPPED || "_" || f_lib_obten_user() CLIPPED || ".log"

    CALL STARTLOG(lc_nombre_log CLIPPED)

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_crea_log : Abre el archivo de bitacora correspondiente al usuario   #
#                  que ejecuta el programa                                  #
#---------------------------------------------------------------------------#
FUNCTION f_lib_crea_log()

    DEFINE
        lc_nom_programa         VARCHAR(15) ,
        lc_nombre_log           VARCHAR(40)

    -- -----------------------------------------------------------------------------

    LET lc_nom_programa = ARG_VAL(0)
    LET lc_nombre_log   = lc_nom_programa CLIPPED || "_" || f_lib_obten_user() CLIPPED || ".log"

    CALL STARTLOG(lc_nombre_log CLIPPED)

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_pregunta : Realiza una pregunta de Si/No, desplegando un mensaje    #
#                  contenido en la variable pc_msg y regresa verdadero o    #
#                  falso dependiendo de la respuesta                        #
#---------------------------------------------------------------------------#
FUNCTION f_lib_pregunta(pc_msg)

    DEFINE
        pc_msg              CHAR(060)

    DEFINE
        lc_enter            CHAR(001)

    DEFINE
        ls_respuesta        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_respuesta    = TRUE
    LET pc_msg          = pc_msg CLIPPED

    WHILE TRUE
        PROMPT pc_msg CLIPPED ATTRIBUTE(REVERSE) FOR lc_enter

        IF lc_enter MATCHES "[sSnN]" THEN
            IF lc_enter MATCHES "[sS]" THEN
                LET ls_respuesta    = TRUE
            ELSE
                LET ls_respuesta    = FALSE
            END IF

            EXIT WHILE
        END IF

    END WHILE

    RETURN ls_respuesta

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_error_msg :Formatea y despliega los mensajes de error en la pantalla#
#---------------------------------------------------------------------------#
FUNCTION f_lib_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(75)

    DEFINE
        lc_enter            CHAR

    -- -----------------------------------------------------------------------------

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR lc_enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_notifica_error : Abre una pantalla indicando que hubo un error      #
#---------------------------------------------------------------------------#
FUNCTION f_lib_notifica_error(pc_msg_error)

    DEFINE
        pc_msg_error            CHAR(100)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_msg AT 8,12 WITH 10 ROWS, 60 COLUMNS
        ATTRIBUTE(BORDER, PROMPT LINE LAST -1)

    DISPLAY "                       A V I S O                       " AT 1,5 ATTRIBUTES(REVERSE)
    DISPLAY "               SE HAN PRESENTADO ERRORES               " AT 3,5
    DISPLAY pc_msg_error CLIPPED AT 4,3
    CALL f_lib_error_msg(" ")

   CLOSE WINDOW win_msg

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_valida_fechas : Valida que la fecha capturada no sea nula y que     #
#                       cumpla las condiciones necesarias para ser aceptada #
#                                                                           #
#   La funcion regresa los siguientes datos:                                #
#       estado    : 0 - No Hubo ningun error                                #
#                   1 - Existio algun error en la validacion                #
#                                                                           #
#       mensaje     : Un mensaje de error indicado la falla encontrada      #
#---------------------------------------------------------------------------#
FUNCTION f_lib_valida_fechas(pdt_fecha)

    DEFINE
        pdt_fecha     DATE

    DEFINE lr_datos RECORD
        estado          SMALLINT    ,
        mensaje         CHAR(100)
    END RECORD 

    -- -----------------------------------------------------------------------------

    LET lr_datos.estado = 0

    IF pdt_fecha IS NULL THEN
        LET lr_datos.mensaje = "LA FECHA NO DEBE SER NULA"
        LET lr_datos.estado = 1
    ELSE
        IF pdt_fecha < "01/01/1980" THEN
            LET lr_datos.mensaje = "FECHA INVALIDA"
            LET lr_datos.estado = 1
        END IF
    END IF

    RETURN lr_datos.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_borra_lineas : Dado un archivo y su ruta de ubicacion elimina las   #
#                      lineas en blanco que contenga al final de este       #
#---------------------------------------------------------------------------#
FUNCTION f_lib_borra_lineas(pc_ruta, pc_archivo)

    DEFINE
        pc_archivo              ,
        pc_ruta                 CHAR(100)

    DEFINE
        lc_comando              CHAR(600)   ,
        lc_ruta_tot             CHAR(300)   ,
        lc_tmp_file             CHAR(300)

    -- -----------------------------------------------------------------------------

    LET lc_tmp_file     = pc_ruta CLIPPED, "/", "dummy_file.out"
    LET lc_ruta_tot     = pc_ruta CLIPPED, "/", pc_archivo CLIPPED 

    LET lc_comando    = "sed -e '/^$/d' ", lc_ruta_tot CLIPPED, " > ", lc_tmp_file CLIPPED
    RUN lc_comando

    LET lc_comando    = " "
    LET lc_comando    = "mv ",lc_tmp_file CLIPPED," ",lc_ruta_tot CLIPPED
    RUN lc_comando
   
    LET lc_comando    = " "
    LET lc_comando    = "chmod 777 ", lc_ruta_tot CLIPPED
    RUN lc_comando    

    LET lc_comando    = " "
    LET lc_comando    = " rm -f ", lc_tmp_file CLIPPED
    RUN lc_comando    

END FUNCTION
