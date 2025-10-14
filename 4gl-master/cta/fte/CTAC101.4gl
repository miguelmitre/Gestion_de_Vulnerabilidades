#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa          => CTAC101 Consulta "Indicadores de Control Cuenta"      #
#Fecha actualiz.   => 16 de Noviembre de 2004                               #
#Actualizacion     => EDUARDO J. RESENDIZ MEDINA                            #
#Sistema           => AFI / CTA                                             #
#############################################################################

DATABASE safre_af

GLOBALS   
    DEFINE v_n_seguro           CHAR(11),
           v_desc_ind_act       CHAR(30),
           v_desc_ind_sdocero   CHAR(30),
           v_desc_marc_56       CHAR(30),
           v_desc_ind_transf    CHAR(30), 
           v_desc_criterio_edad CHAR(15),
           hora                 DATE,
           hoy                  DATE

    DEFINE gr_maeafiliado RECORD
           n_seguro        LIKE afi_mae_afiliado.n_seguro,
           n_unico         LIKE afi_mae_afiliado.n_unico,           
           paterno         LIKE afi_mae_afiliado.paterno,
           materno         LIKE afi_mae_afiliado.materno,
           nombres         LIKE afi_mae_afiliado.nombres,
           fena            LIKE afi_mae_afiliado.fena,
           n_rfc           LIKE afi_mae_afiliado.n_rfc
    END RECORD

    DEFINE gr_ctactrcuenta RECORD LIKE cta_ctr_cuenta.*

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS INPUT  WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-M,
    FORM LINE 3
    
    CALL STARTLOG('CTAC101.log')
    CALL inicio()
    CALL proceso_principal()
    LET v_n_seguro = NULL
    
END MAIN

FUNCTION inicio()

    LET hora = TIME
    LET hoy  = TODAY
    LET v_n_seguro = ARG_VAL(1)
    
END FUNCTION #inicio

FUNCTION proceso_principal()

    OPEN WINDOW vent_1 AT 2,2 WITH FORM "CTAC1011" ATTRIBUTE(BORDER)

    DISPLAY "CTAC101       CONSULTA INDICADORES DE CONTROL CUENTA            " AT 3,1 ATTRIBUTE (REVERSE)

    DISPLAY "                             < Ctrl-C > Salir                             " AT 1,1 ATTRIBUTE(CYAN)

    DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    CLEAR FORM    

    IF v_n_seguro IS NOT NULL AND 
       v_n_seguro <> " " THEN
        CALL consulta()
    --END IF
    ELSE
        MENU "Consulta"
            COMMAND "Consulta" "Consulta Indicadores Afiliados"
                CLEAR FORM
                CALL captura()
            COMMAND "Salir" "Salir del Menu Actual"
                EXIT MENU
        END MENU
    END IF

END FUNCTION

FUNCTION captura()
## captura nss

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " <ENTER> Aceptar        (Ctrl-c) Salir              " AT 1,1 ATTRIBUTE(CYAN)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,CYAN)

    LET v_n_seguro = NULL
    INPUT BY NAME v_n_seguro WITHOUT DEFAULTS
          AFTER INPUT
          CALL consulta()
    EXIT INPUT
    END INPUT
 
END FUNCTION

FUNCTION consulta()
#consulta a tabla afi_mae_afiliado--------------------------

DEFINE vresp     CHAR(1)

           INITIALIZE gr_maeafiliado TO NULL
           INITIALIZE gr_ctactrcuenta TO NULL

           SELECT n_seguro,n_unico,paterno,materno,nombres,fena,n_rfc
           INTO   gr_maeafiliado.*
           FROM   afi_mae_afiliado
           WHERE  n_seguro = v_n_seguro

    ### Valida que el numero de seguro exista en afi_mae_afiliado
           IF SQLCA.SQLCODE = NOTFOUND THEN
               ERROR "NO SE ENCONTRARON REGISTROS CON LAS CONDICIONES"
               SLEEP 2
               ERROR " "
               CLEAR FORM
               RETURN
           ELSE
               CALL encuentra_ctactrcuenta()
           END IF

           LET v_desc_ind_sdocero = ""
           IF gr_ctactrcuenta.ind_saldo_cero IS NULL OR
              gr_ctactrcuenta.ind_saldo_cero = 0 THEN           
               LET v_desc_ind_sdocero = "CUENTA SIN MARCA SALDO CERO"
           ELSE
               LET v_desc_ind_sdocero = "CUENTA CON SALDO CERO "
           END IF

           LET v_desc_ind_act = ""
           IF gr_ctactrcuenta.ind_actividad IS NULL OR
              gr_ctactrcuenta.ind_actividad = 0 THEN
               LET v_desc_ind_act = "CUENTA INACTIVA"
           ELSE
               LET v_desc_ind_act = "CUENTA ACTIVA"
           END IF

           LET v_desc_marc_56 = ""
           IF gr_ctactrcuenta.ind_edad IS NULL OR
              gr_ctactrcuenta.ind_edad = 0 THEN
               LET v_desc_marc_56 = "CUENTA SIN MARCA DE 56 AÑOS"
           ELSE
               LET v_desc_marc_56 = "CUENTA CON MARCA DE 56 AÑOS"
           END IF

           LET v_desc_ind_transf = ""
           IF gr_ctactrcuenta.ind_transferencia IS NULL OR
              gr_ctactrcuenta.ind_transferencia = 0 THEN
               LET v_desc_ind_transf = "SIN PROCESO DE TRANSFERENCIA" 
           ELSE
               IF gr_ctactrcuenta.ind_transferencia = 1 THEN
                   LET v_desc_ind_transf = "EN PROC. TRANSFERENCIA DE REC."
               END IF
               IF gr_ctactrcuenta.ind_transferencia = 2 THEN
                   LET v_desc_ind_transf = "TRANS. CONCLUIDA POR PROCESO"
               END IF
               IF gr_ctactrcuenta.ind_transferencia = 3 THEN
                   LET v_desc_ind_transf = "TRANS. CONCLUIDA POR SOLICITUD"
               END IF
           END IF
           
           LET v_desc_criterio_edad = ""
           IF gr_ctactrcuenta.criterio_edad IS NULL OR
              gr_ctactrcuenta.criterio_edad = "" THEN
              LET v_desc_criterio_edad = "SIN CRITERIO"
           ELSE
              IF gr_ctactrcuenta.criterio_edad = "1" THEN
                 LET v_desc_criterio_edad = "CURP"
              END IF
              IF gr_ctactrcuenta.criterio_edad = "2" THEN
                 LET v_desc_criterio_edad = "RFC"
              END IF
              IF gr_ctactrcuenta.criterio_edad = "3" THEN
                 LET v_desc_criterio_edad = "FECHA NACIMIENTO"
              END IF
           END IF

CALL despliega()
END FUNCTION #consulta

FUNCTION despliega()
DEFINE vresp1   CHAR(1)

           DISPLAY BY NAME v_n_seguro,           
                           gr_maeafiliado.n_unico,                           
                           gr_maeafiliado.paterno,
                           gr_maeafiliado.materno,
                           gr_maeafiliado.nombres,
                           gr_maeafiliado.fena,
                           gr_maeafiliado.n_rfc,
                           gr_ctactrcuenta.ind_saldo_cero,
                           gr_ctactrcuenta.fecha_saldo_cero,
                           gr_ctactrcuenta.ind_actividad,
                           gr_ctactrcuenta.fecha_actividad,
                           gr_ctactrcuenta.ind_edad,
                           gr_ctactrcuenta.fecha_edad,
                           gr_ctactrcuenta.fecha_ind_transf,
                           gr_ctactrcuenta.criterio_edad,
                           gr_ctactrcuenta.ind_transferencia,
                           v_desc_ind_act,
                           v_desc_ind_sdocero,
                           v_desc_marc_56,
                           v_desc_ind_transf,
                           v_desc_criterio_edad

           PROMPT ""

           FOR vresp1
                ATTRIBUTE (REVERSE)

           RETURN

END FUNCTION #despliega

FUNCTION encuentra_ctactrcuenta()
### Si el numero de seguro existe, entonces ejecuta la consulta a cta_ctr_cuenta

   SELECT * INTO   gr_ctactrcuenta.*
   FROM   cta_ctr_cuenta
   WHERE  nss = v_n_seguro

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "NO SE ENCONTRARON REGISTROS EN CTA_CTR_CUENTA"
      SLEEP 2
      ERROR " "
      CLEAR FORM
      RETURN
   END IF

END FUNCTION #encuentra_ctactrcuenta
