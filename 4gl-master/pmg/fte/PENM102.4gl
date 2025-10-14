#################################################################################
#Owner             => E.F.P.                                                    #
#Programa PENM102  => MANTENIMIENTO DEL CATÁLOGO DE MONTOS HISTORICOS DE PMG    #
#Fecha creacion    => 7 DE MARZO DE 2011                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 5 DE FEBRERO DE 2013                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se incluye la actualizacion para el registro del monto    #
#                     mensual a pagar con el 11%                                #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_param_dis   RECORD LIKE seg_modulo.*

    DEFINE 
        aux_pausa     CHAR(1),
        sw_1          SMALLINT,
        hoy           DATE,
        pos           INTEGER,
        sel_where     CHAR(30000),
        cursor1       CHAR(30000),
        cursor2       CHAR(30000),
        cursor3       CHAR(30000),
        select_cur    CHAR(30000),
        cla_where     CHAR(30000),
        g_impre       CHAR(300),
        g_lista       CHAR(300),
        enter         CHAR(1)

    DEFINE g_reg RECORD LIKE tab_pmg_historica.*

    DEFINE gr_historico ARRAY[300] OF RECORD LIKE tab_pmg_historica.*

END GLOBALS

MAIN

    OPTIONS 
        PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-O
        DEFER INTERRUPT
   
    CALL proceso()

END MAIN

###############################################################################
FUNCTION proceso()

   LET HOY = TODAY
   OPEN WINDOW ven_1 AT 3,3 WITH FORM "PENM1021" ATTRIBUTE( BORDER)
   DISPLAY " PENM102            CATALOGO DE MONTOS HISTORICOS DE PMG                           " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "CATALOGO MONTOS PMG"
      COMMAND "Agrega" "Agrega Monto Historico"
         CALL Agrega()
         CLEAR SCREEN
      COMMAND "Consulta" "Consulta Monto Historico"
         CALL Consulta()
         CLEAR SCREEN
      COMMAND "Modifica" "Modifica Monto Historico"
         CALL Modifica()
         CLEAR SCREEN
      COMMAND "Elimina" "Elimina Monto Historico"
         CALL Elimina()
         CLEAR SCREEN
      COMMAND "Salir" "Sale del programa"
         EXIT MENU
   END MENU
   CLOSE WINDOW ven_1

END FUNCTION

###############################################################################
FUNCTION Inicializa()

   LET sw_1 = 0
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*

END FUNCTION

###############################################################################

FUNCTION Agrega()

    DEFINE lr_reg_ant RECORD LIKE tab_pmg_historica.*
        
    DEFINE
        lc_fecha            DATE

    -- -----------------------------------------------------------------------------

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ( ESC ) AGREGA                 (CTRL-C) SALIR                                 " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
    
    INITIALIZE g_reg.* TO NULL
    LET g_reg.fecha_actualiza   = HOY

    SELECT USER
    INTO   g_reg.usuario
    FROM   tab_afore_local

    DISPLAY BY NAME g_reg.fecha_actualiza,
                    g_reg.usuario

    SELECT *
    INTO   lr_reg_ant.*
    FROM   tab_pmg_historica
    WHERE  fecha_hasta IS NULL
   
    LET sw_1 = 0
    
    INPUT BY NAME g_reg.fecha_desde         ,
                  g_reg.importe_mensual     ,
                  g_reg.importe_mensual_11p

        ---------------------------
        AFTER FIELD fecha_desde
        ---------------------------            
            SELECT "X"
            FROM   tab_pmg_historica
            WHERE  fecha_desde = g_reg.fecha_desde
            GROUP BY 1
      
            IF STATUS <> NOTFOUND THEN
               ERROR "FECHA YA INGRESADA"
               NEXT FIELD fecha_desde
            END IF

        ---------------------------
        AFTER FIELD importe_mensual
        ---------------------------
            IF g_reg.importe_mensual <= 0 OR g_reg.importe_mensual IS NULL THEN
               ERROR "EL MONTO DEBE SER MAYOR A CERO"
               NEXT FIELD importe_mensual                    
            END IF

            LET g_reg.incremento_porc = ((g_reg.importe_mensual - lr_reg_ant.importe_mensual)* 100)/lr_reg_ant.importe_mensual
            DISPLAY BY NAME g_reg.incremento_porc

        ---------------------------
        AFTER FIELD importe_mensual_11p
        ---------------------------
            IF g_reg.importe_mensual_11p <= 0 OR g_reg.importe_mensual_11p IS NULL THEN
               ERROR "EL MONTO DEBE SER MAYOR A CERO"
               NEXT FIELD importe_mensual_11p                    
            END IF

        ---------------------------
        ON KEY ( ESC )
        ---------------------------
            IF g_reg.fecha_desde IS NULL THEN
                ERROR "LA FECHA DE INICIO NO PUEDE SER NULA"
                NEXT FIELD fecha_desde
            ELSE
                SELECT "X"
                FROM   tab_pmg_historica
                WHERE  fecha_desde = g_reg.fecha_desde
                GROUP BY 1
                
                IF STATUS <> NOTFOUND THEN
                   ERROR "FECHA YA INGRESADA"
                   NEXT FIELD fecha_desde
                END IF            
            END IF
            
            IF g_reg.importe_mensual <= 0 OR g_reg.importe_mensual IS NULL THEN
               ERROR "EL MONTO DEBE SER MAYOR A CERO"                          
               NEXT FIELD importe_mensual                                      
            END IF                                                             

            IF g_reg.importe_mensual_11p <= 0 OR g_reg.importe_mensual_11p IS NULL THEN
               ERROR "EL MONTO DEBE SER MAYOR A CERO"
               NEXT FIELD importe_mensual_11p                    
            END IF
            
            -- -----------------------------------------------------------------------------
            
            LET lc_fecha    = g_reg.fecha_desde - 1 UNITS DAY
            
            UPDATE tab_pmg_historica
            SET    fecha_hasta  = lc_fecha
            WHERE  fecha_hasta IS NULL
            
            INSERT INTO tab_pmg_historica 
            VALUES (g_reg.*)

            ERROR "REGISTRO INGRESADO" 
            SLEEP 1
            ERROR ""
            CALL Inicializa()
            NEXT FIELD fecha_desde

        ON KEY (INTERRUPT)
           CALL Inicializa()
           EXIT INPUT

    END INPUT

END FUNCTION

###############################################################################
FUNCTION Consulta()

    LET pos = 2
   
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        
        OPEN WINDOW ven_2 AT 6,3 WITH FORM "PENM1022" ATTRIBUTE( BORDER)
        DISPLAY " (ENTER) CONSULTA                                            (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY " PENM102             CATALOGO DE MONTOS HISTORICOS DE PMG                                " AT 3,1 ATTRIBUTE(REVERSE, BOLD)

        DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)
        
        LET INT_FLAG = FALSE
        
        CONSTRUCT cla_where ON fecha_desde, fecha_hasta
                            FROM fecha_desde, fecha_hasta
           ON KEY (control-m)
              ERROR "PROCESANDO INFORMACION..."
              SLEEP 1
              LET int_flag = FALSE
              EXIT CONSTRUCT
           
           ON KEY (control-c)
              LET int_flag = TRUE
              EXIT CONSTRUCT
        
        END CONSTRUCT
        
        IF int_flag = TRUE THEN
           LET int_flag = FALSE
           ERROR "BUSQUEDA CANCELADA..."
           SLEEP 1
           ERROR ""
           CLEAR SCREEN
           CLOSE WINDOW ven_2
           RETURN
        END IF
        
        LET sel_where = "SELECT * ",
                        " FROM tab_pmg_historica WHERE ",
                         cla_where CLIPPED,
                        " ORDER BY 1 DESC "
        
        PREPARE query FROM sel_where
        DECLARE cursor_1 CURSOR FOR query
        
        LET pos = 1
        FOREACH cursor_1 INTO gr_historico[pos].*
           LET pos = pos + 1
        END FOREACH
        
        INITIALIZE gr_historico[pos].* TO NULL
        IF (pos-1) >= 1 THEN
           CALL  SET_COUNT(pos-1)
           ERROR ""
        
           DISPLAY ARRAY gr_historico TO scr_1.*
              
              ON KEY (INTERRUPT)
                 EXIT DISPLAY
           
           END DISPLAY
        
           CLOSE WINDOW ven_2
        ELSE
           ERROR "ARCHIVO DE TIPO DE REGIMEN ... VACIO"
           SLEEP 1
           ERROR ""
           CLOSE WINDOW ven_2
        END IF
    
    END IF
    CLEAR SCREEN

END FUNCTION

###############################################################################

FUNCTION Modifica()

    LET pos = 2
   
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        OPEN WINDOW ven_2 AT 6,3 WITH FORM "PENM1022" ATTRIBUTE(BORDER)
        DISPLAY " (ENTER) CONSULTA                                            (CTRL-C) SALIR      " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY "                Escoja con <ENTER> el Registro a modificar                   " AT 2,1
        DISPLAY " PENM102               CATALOGO DE MONTOS HISTORICOS DE PMG                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
    
        DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)
    
        LET int_flag = FALSE
    
        CONSTRUCT cla_where ON fecha_desde, fecha_hasta
                            FROM fecha_desde, fecha_hasta
           
            ON KEY (CONTROL-M)
               ERROR "PROCESANDO INFORMACION..."
               LET int_flag = FALSE
               EXIT CONSTRUCT
            
            ON KEY (CONTROL-C)
               LET int_flag = TRUE
               EXIT CONSTRUCT
        
        END CONSTRUCT
    
        IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "BUSQUEDA CANCELADA..."
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ven_2
            RETURN
        END IF
        
        LET sel_where = "SELECT * ",
                        " FROM tab_pmg_historica WHERE ",
                         cla_where CLIPPED,
                        " ORDER BY 1 DESC "
    
        PREPARE query1 FROM sel_where
        DECLARE cursor_2 CURSOR FOR query1
    
        LET pos = 1
    
        FOREACH cursor_2 INTO gr_historico[pos].*
            LET pos = pos + 1
        END FOREACH
    
        INITIALIZE gr_historico[pos].* TO NULL
    
        IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
            ERROR ""
            
            DISPLAY ARRAY gr_historico TO scr_1.*
                ON KEY (control-m)
                    LET pos = ARR_CURR()
                    LET g_reg.* = gr_historico[pos].*
                    EXIT DISPLAY
                
                ON KEY (INTERRUPT)
                    ERROR "DEBE ELEGIR UN REGISTRO"
                    LET pos = ARR_CURR()
            
            END DISPLAY
        
           CLOSE WINDOW ven_2
        ELSE
           ERROR "ARCHIVO DE HISTORICO DE PMG VACIO"
           SLEEP 1
           ERROR ""
           CLOSE WINDOW ven_2
           RETURN
        END IF
        
        DISPLAY "" AT 1,1
        DISPLAY "" AT 2,1
        DISPLAY " (CTRL-C) SALIR " AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY BY NAME g_reg.*
        
        INPUT BY NAME g_reg.fecha_desde         ,
                      g_reg.fecha_hasta         ,
                      g_reg.importe_mensual     ,
                      g_reg.importe_mensual_11p WITHOUT DEFAULTS
        
            ---------------------------
            AFTER FIELD fecha_desde
            ---------------------------            
                IF g_reg.fecha_desde IS NULL THEN
                   ERROR "LA FECHA INICIAL NO PUEDE SER NULA"
                   NEXT FIELD fecha_desde                    
                END IF
            
            ---------------------------
            AFTER FIELD importe_mensual
            ---------------------------
                IF g_reg.importe_mensual <= 0 OR g_reg.importe_mensual IS NULL THEN
                   ERROR "EL MONTO DEBE SER MAYOR A CERO"
                   NEXT FIELD importe_mensual                    
                END IF

            ---------------------------
            AFTER FIELD importe_mensual_11p
            ---------------------------
                IF g_reg.importe_mensual_11p <= 0 OR g_reg.importe_mensual_11p IS NULL THEN
                   ERROR "EL MONTO DEBE SER MAYOR A CERO"
                   NEXT FIELD importe_mensual                    
                END IF

            ---------------------------
            ON KEY ( ESC )
            ---------------------------
            CALL Pregunta()
            
            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_pmg_historica 
               SET    fecha_desde           = g_reg.fecha_desde         ,
                      fecha_hasta           = g_reg.fecha_hasta         ,
                      importe_mensual       = g_reg.importe_mensual     ,
                      importe_mensual_11p   = g_reg.importe_mensual_11p ,
                      fecha_actualiza       = HOY                       ,
                      usuario               = USER
               WHERE  fecha_actualiza       = g_reg.fecha_actualiza
               
               ERROR "REGISTRO MODIFICADO"
               SLEEP 1
               ERROR ""
               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICAR CANCELADO"
               SLEEP 1
               ERROR ""
               CALL Inicializa()
            END IF
            
            EXIT INPUT
            
            ON KEY (INTERRUPT)
               CALL Inicializa()
               EXIT INPUT
        
        END INPUT
    ELSE
       ERROR "ARCHIVO DE TIPO DE REGIMEN ... VACIO"
    END IF
    CLEAR SCREEN

END FUNCTION

###############################################################################
FUNCTION Elimina()

    DEFINE
        ldt_max_fecha               DATE

    -- -----------------------------------------------------------------------------

    LET pos = 2
   
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        OPEN WINDOW ven_2 AT 6,3 WITH FORM "PENM1022" ATTRIBUTE(BORDER)
        DISPLAY " (ENTER) CONSULTA                                            (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY "                 Escoja con <ENTER> el Registro a eliminar                     " AT 2,1
        DISPLAY " PENM102               CATALOGO DE MONTOS HISTORICOS DE PMG                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)
        
        LET int_flag = FALSE
        
        CONSTRUCT cla_where ON fecha_desde, fecha_hasta
                              FROM fecha_desde, fecha_hasta
           
            ON KEY (control-m)
               ERROR "PROCESANDO INFORMACION..."
               LET int_flag = FALSE
               EXIT CONSTRUCT
            
            ON KEY (control-c)
               LET int_flag = TRUE
               EXIT CONSTRUCT
        
        END CONSTRUCT
        
        IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "BUSQUEDA CANCELADA..."
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ven_2
            RETURN
        END IF
        
        LET sel_where = "SELECT * ",
                        " FROM tab_pmg_historica WHERE ",
                         cla_where CLIPPED,
                        " ORDER BY 1 DESC "
        
        PREPARE query2 FROM sel_where
        DECLARE cursor_3 CURSOR FOR query2
        
        LET pos = 1
        FOREACH cursor_3 INTO gr_historico[pos].*
            LET pos = pos + 1
        END FOREACH
        
        INITIALIZE gr_historico[pos].* TO NULL
        
        IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
            ERROR ""
            DISPLAY ARRAY gr_historico TO scr_1.*
               
               ON KEY (control-m)
                  LET pos = ARR_CURR()
                  LET g_reg.fecha_desde         = gr_historico[pos].fecha_desde
                  LET g_reg.fecha_hasta         = gr_historico[pos].fecha_hasta
                  LET g_reg.importe_mensual     = gr_historico[pos].importe_mensual 
                  LET g_reg.importe_mensual_11p = gr_historico[pos].importe_mensual_11p 
                  LET g_reg.fecha_actualiza     = gr_historico[pos].fecha_actualiza 
                  LET g_reg.incremento_porc     = gr_historico[pos].incremento_porc 
                  LET g_reg.usuario             = gr_historico[pos].usuario        
                  EXIT DISPLAY
            
               ON KEY (INTERRUPT)
                  ERROR "DEBE ELEGIR UN REGISTRO"
                  LET pos = ARR_CURR()
            
            END DISPLAY
            CLOSE WINDOW ven_2
        ELSE
            ERROR "ARCHIVO DE HISTORICOS VACIO"
            SLEEP 1
            ERROR ""
            CLOSE WINDOW ven_2
            RETURN
        END IF
        
        DISPLAY "" AT 1,1
        DISPLAY "" AT 2,1
        DISPLAY " (CTRL-C) SALIR " AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
        
        DISPLAY BY NAME g_reg.*
        CALL Pregunta()
        
        IF aux_pausa MATCHES "[Ss]" THEN
            DELETE 
            FROM   tab_pmg_historica
            WHERE  fecha_desde = g_reg.fecha_desde

            SELECT MAX(fecha_desde)
            INTO   ldt_max_fecha
            FROM   tab_pmg_historica            
            
            UPDATE tab_pmg_historica
            SET    fecha_hasta  = NULL
            WHERE  fecha_desde  = ldt_max_fecha
           
            ERROR "REGISTRO ELIMINADO"
            SLEEP 1
            ERROR ""
        ELSE
            ERROR "PROCESO CANCELADO"
            SLEEP 1
            ERROR ""
        END IF
        
        CALL Inicializa()
    ELSE
       ERROR "ARCHIVO DE HISTORICOS VACIO"
    END IF
    
    CLEAR SCREEN

END FUNCTION

###############################################################################

FUNCTION Pregunta()
   
   PROMPT "ESTA SEGURO (S/N) ? " FOR CHAR aux_pausa

END FUNCTION
