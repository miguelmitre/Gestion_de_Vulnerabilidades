#############################################################################
#Propietario        => E.F.P.                                               #
#Programa VOLB0021  => REVERSO LIQUIDACION DE APORT. VENTANILLA Y           #
#                      COMPLEMENTARIA                                       #
#                      VENTANILLA                                           #
#Fecha creacion     => 22 JUNIO 2009                                        #
#Por                => STEFANIE DANIELA VERA PIÑA                           #
#Fecha actualiz.    => 16 DICIMEBRE 2011                                    #
#Por                => David Miguel Garibay Rivera                          #
#Descripción                                                                #
#Modificaciones     => Se graga las modificaciones necesarias para eliminar #
#                      la liquidación de un reintegro (subcuenta = 0 en la  #
#                      tabla "int_det_voluntaria")                          #
#############################################################################
DATABASE safre_af
    DEFINE
        HOY         DATE

    DEFINE
        enter       CHAR(1)

    DEFINE
        v_folio     INTEGER

GLOBALS

END GLOBALS

MAIN
DEFINE 
   lsi_bndReversa  SMALLINT --Esta bandera indica si el usuario 
                            --desea ejecutar el proceso de 
                            --"reversa de liquidación"

    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT
        
    --Inicialización de variables
    LET lsi_bndReversa = 0

    CALL STARTLOG("INTB03011.log")
    CALL inicio()

    OPEN WINDOW v1 AT 3,3 WITH FORM "VOLB00211" ATTRIBUTE( BORDER )
    DISPLAY " <ESC> Ejecutar                                              <Ctrl-C> Salir    " AT 1,1
    DISPLAY " VOLB0021      REVERSO LIQUIDACION DE APORTACIONES VOLUNTARIAS                 " AT 2,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE (REVERSE)

    INPUT BY NAME v_folio WITHOUT DEFAULTS
        AFTER FIELD v_folio
            IF v_folio IS NULL THEN
                ERROR " DIGITE CORRECTAMENTE EL FOLIO "
                NEXT FIELD v_folio
            END  IF

            SELECT "a.x" FROM dis_cuenta a
            WHERE  a.folio = v_folio
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR " NO EXISTE INFORMACION DE ESE FOLIO "
                NEXT FIELD v_folio
            END IF
            
            --El usuario si desea ejecutar el proceso de reserva
            LET lsi_bndReversa = 1

        ON KEY (ESC)

            IF v_folio IS NULL OR v_folio = 0 THEN
                ERROR " DIGITE CORRECTAMENTE EL FOLIO "
                NEXT FIELD v_folio
            END IF

            SELECT "a.x" FROM dis_cuenta a
            WHERE  a.folio = v_folio
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR " NO EXISTE INFORMACION DE ESE FOLIO "
                NEXT FIELD v_folio
            END IF
            --El usuario si desea ejecutar el proceso de reserva
            LET lsi_bndReversa = 1

            EXIT INPUT

        ON KEY( INTERRUPT, CONTROL-C )
            --El usuario no desea ejecutar el proceso de reserva
            LET lsi_bndReversa = 0
            
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
            FOR enter
            
            EXIT INPUT

    END INPUT

    --El usuario si desea ejecutar el proceso de reserva
    IF (lsi_bndReversa = 1) THEN
       CALL reversar()

       PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR "
       FOR CHAR enter
       EXIT PROGRAM
    END IF

    CLOSE WINDOW v1
END MAIN


FUNCTION inicio()
#----------------

    LET HOY = TODAY

END FUNCTION


FUNCTION reversar()
DEFINE 
   lr_voluntaria RECORD LIKE int_det_voluntaria.*

    --Regresa el registro de la tabla "int_det_voluntaria"
    CALL f_regresaVoluntaria(v_folio, '01', 2)
         RETURNING lr_voluntaria.*

    DELETE
    FROM  dis_cuenta
    WHERE folio = v_folio

    DELETE
    FROM  cta_saldo_vol
    WHERE folio = v_folio

    UPDATE int_det_voluntaria
    SET    estado = 1
    WHERE  folio = v_folio        --Numero de folio
    AND    resul_operacion = '01' --
    AND    estado = 2             --Liquidado
    
    --SI se trata de un reintegro,
    --entonces también se llevan a cabo las siguientes acciones
    IF (lr_voluntaria.tipo_aportacion = "R") THEN
    
        --Elimina los registros de la tabla "ret_notifica_devol"
        DELETE 
          FROM ret_notifica_devol
         WHERE nss = lr_voluntaria.nss
          AND  mto_reintegro = lr_voluntaria.monto_neto
          AND  estado_solicitud = 80  #-- LIQUIDADO --#
          
       --Actualiza los valores originales de la tabla "ret_mto_devol"
       UPDATE ret_mto_devol
          SET estado_solicitud = 50   #-- RECIBIDO --#
        WHERE nss = lr_voluntaria.nss
          AND mto_reintegro = lr_voluntaria.monto_neto
          AND estado_solicitud = 80   #-- LIQUIDADO --#
    END IF
END FUNCTION


FUNCTION f_regresaVoluntaria(li_numFolio, lc_operacion, li_estatus)
DEFINE
   li_numFolio   INTEGER,                         --Numero de folio
   lc_operacion  CHAR(2),                         --Tipo de operacion
   li_estatus    INTEGER,                         --Estatus del registro
                                                  -- 1 Capturado
                                                  -- 2 Liquidado
   lc_qryVol     CHAR(200),                       --Esta variable opermitira extraer 
                                                  --la informacion de la tabla "int_det_voluntaria"
   lr_voluntaria RECORD LIKE int_det_voluntaria.* -- Registro de la tabla "int_det_voluntaria"
   
   --Inicializacion de variables   
   INITIALIZE lr_voluntaria.* TO NULL
   LET lc_qryVol = "SELECT FIRST 1 *\n",
                   "  FROM int_det_voluntaria\n",
                   " WHERE folio = ? \n",
                   "   AND resul_operacion = ?\n",
                   "   AND estado = ?\n",
                   "   AND tipo_aportacion = 'R' \n"    --REINTEGRO
                   
   PREPARE prp_regVol FROM lc_qryVol
   EXECUTE prp_regVol USING li_numFolio, 
                            lc_operacion, 
                            li_estatus
                       INTO lr_voluntaria.*
                       
   RETURN lr_voluntaria.*

END FUNCTION