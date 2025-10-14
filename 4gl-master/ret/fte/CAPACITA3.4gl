################################################################################
#Owner             => E.F.P.                                                   #
#Programa CAPACITA => PREPARA AMBIENTE PARA CAPACITACION DE RETIRO AFORE HSBC  #
#Fecha creacion    => 09 DE SEPTIEMBRE DEL 2004                                #
#By                => VERONICA LOPEZ SANCHEZ                                   #
#Fecha actualiz.   =>                                                          #
#Actualizacion     =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        ret_iv_rt             CHAR(1) ,
        ret_parcial           CHAR(1) ,
	ret_pen_aut_imss      CHAR(1) ,
	ret_plan_privado      CHAR(1) ,
        ret_tram_judicial     CHAR(1) , 
        ret_sar92             CHAR(1) ,
        genera_archivo        CHAR(1) ,
        ret_neg_pension       CHAR(1) ,
        ret_reingreso         CHAR(1) ,
        ret_transferencia     CHAR(1) ,
	aclaraciones          CHAR(1) ,
        voluntarias           CHAR(1) ,
        global                CHAR(1)
    END RECORD

    DEFINE #glo #param_ret
        g_param_ret           RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(001) ,
        cp                    CHAR(200) ,
        ch                    CHAR(220)

    DEFINE #glo #integer
        i                     ,
        i_folio               INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT  WRAP       ,
        PROMPT LINE LAST  ,
        ACCEPT KEY CONTROL-I

    CALL init()

    OPEN WINDOW capacita3 AT 2,2 WITH FORM "CAPACITA31" ATTRIBUTE(BORDER)
    DISPLAY " CAPACITA3         AMBIENTE PARA CAPACITACION - RETIROS                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "   ESC <Ejecutar>                                      < Ctrl-C > Salir        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        INPUT BY NAME reg_1.* WITHOUT DEFAULTS

            ON KEY(ESC)
                #pp
                IF reg_1.ret_iv_rt = "X" THEN
                    CALL ret_iv_rt() #rir
                END IF

                IF reg_1.ret_parcial = "X" THEN
                    CALL ret_parcial() #rp
                END IF

                IF reg_1.ret_pen_aut_imss = "X" THEN
                    CALL tipo_retiroE() #trE
                END IF

                IF reg_1.ret_plan_privado = "X" THEN
                    CALL tipo_retiroF() #trF
                END IF

                IF reg_1.ret_tram_judicial = "X" THEN
                    CALL tipo_retiroG() #trG
                END IF

                IF reg_1.ret_sar92 = "X" THEN
                    CALL tipo_retiroH() #trH
                END IF
                
                IF reg_1.genera_archivo = "X" THEN
                    CALL genera_archivo() #ga
                END IF

                IF reg_1.ret_neg_pension = "X" THEN
                    CALL tipo_retiroD() #trD
                END IF

                IF reg_1.ret_reingreso = "X" THEN
                    CALL tipo_retiroJ() #trJ
                END IF
    
                IF reg_1.ret_transferencia = "X" THEN
                    CALL tipo_retiroABC() #trABC
                END IF

                IF reg_1.aclaraciones = "X" THEN
                    CALL aclaraciones() #a
                END IF

                IF reg_1.voluntarias = "X" THEN
                    CALL voluntarias() #v
                END IF

                IF reg_1.global = "X" THEN
                    CALL ini_global() #ig
                END IF
                EXIT INPUT
                
            ON KEY(INTERRUPT)
                PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
        END INPUT  
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR" FOR CHAR enter
    CLOSE WINDOW capacita3
END MAIN


FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT *
    INTO   g_param_ret.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'
END FUNCTION


FUNCTION genera_archivo()
#ga---------------------
    DELETE 
    FROM   dis_cuenta
    WHERE  nss IN ("11755861405","01674850563","01685062711")

    LOAD FROM "dis_cuenta.TH.unl" INSERT INTO dis_cuenta

    DELETE 
    FROM   ret_sal_his_rx
    WHERE  nss IN ("01664822127","01674850563","01685062711","11755861405",
                   "01693914887")

    DELETE 
    FROM   ret_sal_his_tx
    WHERE  nss IN ("01664822127","01674850563","01685062711","11755861405",
                   "01693914887")

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP19"

    LET cp = "cp OP19 ",g_param_ret.ruta_rescate
    RUN cp

    LET ch = "chmod 777 ",g_param_ret.ruta_rescate CLIPPED,"/OP19"
    RUN ch
END FUNCTION


{
FUNCTION ret_iv_rt()
#rir----------------
    DEFINE #loc #integer
        s_folio               SMALLINT

    DECLARE cur_1 CURSOR FOR
    SELECT folio
    FROM   ret_cza_pe03

    FOREACH cur_1 INTO s_folio
        DELETE
        FROM  ret_cza_lote
        WHERE folio = s_folio

        DELETE
        FROM  ret_seguimiento
        WHERE folio = s_folio
    END FOREACH

    DELETE
    FROM  ret_cza_pe03;

    DELETE
    FROM  ret_his_det_pe03;

    DELETE
    FROM  ret_sum_pe03;

    DELETE
    FROM  ret_cza_pe05;

    DELETE
    FROM  ret_his_det_pe05;

    DELETE
    FROM  ret_sum_pe05;

    DELETE
    FROM  afi_mae_afiliado
    WHERE n_seguro IN("04695322851","54917801448","54843810166")

    LOAD FROM "maeafili_ivrt.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM  dis_cuenta 
    WHERE nss IN("04695322851","54917801448","54843810166")
    AND   tipo_movimiento BETWEEN 401 AND 412

    LOAD FROM "cuenta_ivrt.unl" INSERT INTO dis_cuenta

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss in ("04695322851","54843810166","54917801448")

    LOAD FROM "ctr_cuenta_ivrt.unl" INSERT INTO cta_ctr_cuenta
END FUNCTION
}

FUNCTION ret_iv_rt()
#rir----------------
    DEFINE #loc #integer
        s_folio               SMALLINT

    DECLARE cur_1 CURSOR FOR
    SELECT folio
    FROM   ret_cza_pe03

    FOREACH cur_1 INTO s_folio
        DELETE
        FROM  ret_cza_lote
        WHERE folio = s_folio

        DELETE
        FROM  ret_seguimiento
        WHERE folio = s_folio
    END FOREACH

    DELETE
    FROM  ret_consecutivo
    WHERE consecutivo > 28
   
    INSERT INTO  ret_consecutivo
    VALUES (29)

    DELETE
    FROM  ret_cza_pe03;

    DELETE
    FROM  ret_his_det_pe03;

    DELETE
    FROM  ret_sum_pe03;

    DELETE
    FROM  ret_cza_pe05;

    DELETE
    FROM  ret_his_det_pe05;

    DELETE
    FROM  ret_sum_pe05;

    DELETE
    FROM  cta_act_marca
    WHERE  nss in ("32755814657",
                   "01765500770",
                   "01755565817",
                   "32786227366",
                   "24773901582");
    DELETE
    FROM  cta_his_marca
    WHERE  nss in ("32755814657",
                   "01765500770",
                   "01755565817",
                   "32786227366",
                   "24773901582");

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss in ("32755814657",
                   "01765500770",
                   "01755565817",
                   "32786227366",
                   "24773901582");

    LOAD FROM "ctr2.unl" INSERT INTO cta_ctr_cuenta

    DELETE 
    FROM dis_cuenta
    WHERE  nss in ("32755814657",
                   "01765500770",
                   "01755565817",
                   "32786227366",
                   "24773901582");

    LOAD FROM "dis_cuenta_ivrt.unl" INSERT INTO  dis_cuenta

END FUNCTION


FUNCTION ret_parcial()   -- VLS
#rp-------------------

    OPTIONS MESSAGE LINE LAST

    MESSAGE "PROCESANDO..." ATTRIBUTE(REVERSE)

    -- OK -------------------------------------------------------

    DELETE 
    FROM   ret_parcial_resol
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

    LOAD FROM "ret_parcial_resol_coppel1.unl" INSERT INTO ret_parcial_resol;
    ---LOAD FROM "ret_parcial_resol_coppel.unl"  INSERT INTO ret_parcial_resol;

    DELETE 
    FROM   ret_cza_resol
    WHERE  nom_archivo in ("OP07-I","OP07-1")

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP13-I"

    ---DELETE
    ---FROM   ret_monto_siefore
    ---WHERE  nss IN ("76784200024","01573870720","32584152022","23604310773","11755533178")
    ---AND    tipo_retiro = "I"


    DELETE FROM afi_mae_afiliado
    WHERE  n_seguro IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

    LOAD FROM "afi_mae_afiliado_parcial.unl" INSERT INTO afi_mae_afiliado;
    LOAD FROM "afi_mae_afiliado_coppel.unl" INSERT INTO afi_mae_afiliado

    DELETE FROM ret_parcial_tx
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

    DELETE FROM ret_parcial 
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

    ---LOAD FROM  "ret_parcial_coppel_capt.unl" INSERT INTO ret_parcial;
    ---LOAD FROM "ret_parcial_certificar.unl" INSERT INTO ret_parcial;
    LOAD FROM "ret_parcial_certificar1.unl" INSERT INTO ret_parcial;
    LOAD FROM  "ret_parcial_mat_coppel.unl" INSERT INTO ret_parcial;
    ---LOAD FROM  "ret_parcial_coppel2.unl" INSERT INTO ret_parcial;
    ---LOAD FROM "ret_parcial_coppel.unl" INSERT INTO ret_parcial;

    ---LOAD FROM "ret_parcial_coppel.unl" INSERT INTO ret_parcial;

    DELETE FROM dis_cuenta
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')
		
    LOAD FROM "dis_cuenta_parcial.unl" INSERT INTO dis_cuenta


    DELETE FROM ret_his_det_av13
    WHERE  n_seguro IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

    DELETE FROM cta_act_marca
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

    DELETE FROM cta_his_marca
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

    LOAD FROM "cta_ctr_cuenta_parcial_vero.unl" INSERT INTO cta_ctr_cuenta;
    LOAD FROM "cta_ctr_cuenta_coppel.unl" INSERT INTO cta_ctr_cuenta;
    ---LOAD FROM "cta_act_marca_coppel.unl" INSERT INTO cta_act_marca; 
    ---LOAD FROM "cta_his_marca_coppel.unl" INSERT INTO cta_his_marca;

    DELETE 
    FROM   ret_monto_siefore
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')
    
    -------------------------------------------------------------

    DELETE
    FROM   ret_ctr_envio
    WHERE  fecha_envio = HOY

    DELETE
    FROM   ret_beneficiario
    WHERE  nss IN ('11906942054','24977243443','23734704028','43008360505',                        '43008479073','43028495117','47897000312','67028323979')

END FUNCTION


FUNCTION tipo_retiroE()
#trE-------------------
    DELETE 
    FROM   dis_cuenta
    WHERE  nss = "63856801301";

    LOAD FROM "dis_cuentaE.unl" INSERT INTO dis_cuenta

    DELETE 
    FROM   afi_mae_siefore
    WHERE  n_seguro = "63856801301";

    LOAD FROM "afi_mae_sieforeE.unl" INSERT INTO afi_mae_siefore

    DELETE
    FROM   afi_mae_afiliado
    WHERE  n_seguro = "63856801301";

    LOAD FROM "afi_mae_afiliadoE.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM   cta_act_marca
    WHERE  nss = "63856801301";

    DELETE
    FROM   cta_his_marca
    WHERE  nss = "63856801301";

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss = "63856801301";

    LOAD FROM "cta_ctr_cuentaE.unl" INSERT INTO cta_ctr_cuenta;
   # LOAD FROM "cta_his_marcaE.unl" INSERT INTO cta_his_marca
   # LOAD FROM "cta_act_marcaE.unl" INSERT INTO cta_act_marca

    DELETE
    FROM   ret_consecutivo
    WHERE  consecutivo = 19;

    LOAD FROM "ret_consecutivoE.unl" INSERT INTO ret_consecutivo

    DELETE
    FROM   ret_solicitud_rx
    WHERE  nss = "63856801301";

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP06-E";

    DELETE
    FROM   ret_solicitud_tx
    WHERE  nss        = "63856801301"
    AND    tipo_retiro = "E";

    #LOAD FROM "ret_solicitud_txE.unl" INSERT INTO ret_solicitud_tx

    DELETE
    FROM   ret_solicitud_rx
    WHERE  folio IN(SELECT folio
                    FROM   ret_cza_lote
                    WHERE  nom_archivo = "OP06-E"
                   )

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro = "E"
    AND    estado      = 2

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro  = "E"
    AND    fecha_genera = TODAY

    DELETE
    FROM   dis_provision
    WHERE  nss = "63856801301"

    LET cp = "cp OP06-E ",g_param_ret.ruta_rescate
    RUN cp

    LET ch = "chmod 777 ",g_param_ret.ruta_rescate CLIPPED,"/OP06-E"
    RUN ch
END FUNCTION


FUNCTION tipo_retiroF()
#trF-------------------
    DELETE 
    FROM   dis_cuenta
    WHERE  nss = "32624452416";

    LOAD FROM "dis_cuentaF.unl" INSERT INTO dis_cuenta

    DELETE 
    FROM   afi_mae_siefore
    WHERE  n_seguro = "32624452416";

    LOAD FROM "afi_mae_sieforeF.unl" INSERT INTO afi_mae_siefore

    DELETE
    FROM   afi_mae_afiliado
    WHERE  n_seguro = "32624452416";

    LOAD FROM "afi_mae_afiliadoF.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM   cta_act_marca
    WHERE  nss = "32624452416";

    DELETE
    FROM   cta_his_marca
    WHERE  nss = "32624452416";

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss = "32624452416";

    LOAD FROM "cta_ctr_cuentaF.unl" INSERT INTO cta_ctr_cuenta;
   # LOAD FROM "cta_his_marcaF.unl" INSERT INTO cta_his_marca
   # LOAD FROM "cta_act_marcaF.unl" INSERT INTO cta_act_marca

    DELETE
    FROM   ret_consecutivo
    WHERE  consecutivo = 21;

    LOAD FROM "ret_consecutivoF.unl" INSERT INTO ret_consecutivo

    DELETE
    FROM   ret_solicitud_rx
    WHERE  nss = "32624452416";

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP06-F";

    DELETE
    FROM   ret_solicitud_tx
    WHERE  nss        = "32624452416"
    AND    tipo_retiro = "F";

    #LOAD FROM "ret_solicitud_txF.unl" INSERT INTO ret_solicitud_tx

    DELETE
    FROM   ret_solicitud_rx
    WHERE  folio IN(SELECT folio
                    FROM   ret_cza_lote
                    WHERE  nom_archivo = "OP06-F"
                   )

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro = "F"
    AND    estado      = 2

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro  = "F"
    AND    fecha_genera = TODAY

    DELETE
    FROM   dis_provision
    WHERE  nss = "32624452416"

    LET cp = "cp OP06-F ",g_param_ret.ruta_rescate
    RUN cp

    LET ch = "chmod 777 ",g_param_ret.ruta_rescate CLIPPED,"/OP06-F"
    RUN ch
END FUNCTION


FUNCTION tipo_retiroG()
#trG-------------------
    DELETE 
    FROM  ret_solicitud_rx
    WHERE nss = "01573870720";

    DELETE
    FROM  ret_solicitud_tx
    WHERE nss = "01573870720";
 
    DELETE
    FROM  dis_cuenta
    WHERE nss = "01573870720";

    DELETE
    FROM  dis_provision 
    WHERE nss = "01573870720";

    DELETE
    FROM  cta_act_marca
    WHERE nss = "01573870720";

    DELETE
    FROM  cta_his_marca
    WHERE nss = "01573870720";

    DELETE
    FROM  afi_mae_afiliado
    WHERE n_seguro = "01573870720";

    DELETE
    FROM  afi_domicilio
    WHERE nss = "01573870720";

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo IN("OP06-G");

    DELETE
    FROM  cta_ctr_cuenta
    WHERE nss = "01573870720";

    DELETE
    FROM  ret_ctr_envio_lote
    WHERE tipo_retiro = "G"
    AND   fecha_envio = TODAY;

    SELECT "OK"
    FROM   ret_consecutivo 
    WHERE  consecutivo = 520275

    IF STATUS = NOTFOUND THEN
        INSERT INTO ret_consecutivo
        VALUES (520275);
    END IF

    LOAD FROM "G_dis_cuenta.unl"
    INSERT INTO dis_cuenta;

    LOAD FROM "G_ret_solicitud_tx.unl"
    INSERT INTO ret_solicitud_tx;

    LOAD FROM "G_afi_mae_afiliado.unl"
    INSERT INTO afi_mae_afiliado;

    LOAD FROM "G_afi_domicilio.unl"
    INSERT INTO afi_domicilio;

    LOAD FROM "G_cta_ctr_cuenta.unl"
    INSERT INTO cta_ctr_cuenta;

    LOAD FROM "G_cta_act_marca.unl"
    INSERT INTO cta_act_marca;

    LOAD FROM "G_cta_his_marca.unl"
    INSERT INTO cta_his_marca;

    UPDATE cta_act_marca
    SET    fecha_ini = TODAY 
    WHERE  nss = "01573870720";

    UPDATE cta_his_marca
    SET    fecha_ini = TODAY 
    WHERE  nss = "01573870720";
END FUNCTION


FUNCTION tipo_retiroH()
#trH-------------------
    DELETE
    FROM   dis_cuenta 
    WHERE  nss IN ("01007602749","01018310175","01674223845")

    LOAD FROM "dis_cuentaveroH.unl" INSERT INTO dis_cuenta

    DELETE
    FROM   afi_mae_siefore
    WHERE  n_seguro IN ("01007602749","01018310175","01674223845");

    LOAD FROM "afi_mae_sieforeveroH.unl" INSERT INTO afi_mae_siefore

    DELETE
    FROM   afi_mae_afiliado
    WHERE  n_seguro IN ("01007602749","01018310175","01674223845"); 

    LOAD FROM "afi_mae_afiliadoveroH.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM   cta_act_marca
    WHERE  nss IN ("01007602749","01018310175","01674223845");

    DELETE 
    FROM   cta_his_marca
    WHERE  nss IN ("01007602749","01018310175","01674223845");

    DELETE  
    FROM   cta_ctr_cuenta
    WHERE  nss IN ("01007602749","01018310175","01674223845");

    LOAD FROM "cta_ctr_cuentaveroH.unl" INSERT INTO cta_ctr_cuenta;
    LOAD FROM "cta_his_marcaveroH.unl" INSERT INTO cta_his_marca; 
    LOAD FROM "cta_act_marcaveroH.unl" INSERT INTO cta_act_marca;

    DELETE
    FROM   ret_consecutivo
    WHERE  consecutivo = 18

    LOAD FROM "ret_consecutivoH.unl" INSERT INTO ret_consecutivo

    DELETE 
    FROM   ret_solicitud_rx
    WHERE  nss IN ("01007602749","01018310175","01674223845");

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP06-H"

    UPDATE ret_solicitud_tx
    SET  estado_solicitud   = 3 ,
         acciones_ret97     = 0 ,
         acciones_cuota_soc = 0 ,
         acciones_cv        = 0 ,
         acciones_ret92     = 0 ,
         saldo_viv97        = 0 ,
         saldo_viv92        = 0 ,
         rechazo_cod        = 0
    WHERE  tipo_prestacion  = 9
    AND    tipo_retiro      = "H"
    AND    nss IN ("01018310175","01007602749","01674223845")


    DELETE
    FROM   ret_solicitud_rx
    WHERE  folio IN(SELECT folio
                    FROM   ret_cza_lote
                    WHERE  nom_archivo = "OP06-H"
                   )

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro = "H"
    AND    estado      = 2

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro  = "H"
    AND    fecha_genera = TODAY

    DELETE
    FROM   dis_provision 
    WHERE  nss IN ("01007602749","01018310175","01674223845")

    LET cp = "cp OP06-H ",g_param_ret.ruta_rescate
    RUN cp

    LET ch = "chmod 777 ",g_param_ret.ruta_rescate CLIPPED,"/OP06-H"
    RUN ch
END FUNCTION


FUNCTION tipo_retiroD()
#trD-------------------
    DELETE
    FROM   dis_cuenta 
    WHERE  nss IN ("49924106823","31713900129","32584152022")

    LOAD FROM "dis_cuentaD.unl" INSERT INTO dis_cuenta

    DELETE
    FROM   afi_mae_siefore
    WHERE  n_seguro IN ("49924106823","31713900129","32584152022");

    LOAD FROM "afi_mae_sieforeD.unl" INSERT INTO afi_mae_siefore

    DELETE
    FROM   afi_mae_afiliado
    WHERE  n_seguro IN ("49924106823","31713900129","32584152022"); 

    LOAD FROM "afi_mae_afiliadoD.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM   cta_act_marca
    WHERE  nss IN ("49924106823","31713900129","32584152022");

    DELETE 
    FROM   cta_his_marca
    WHERE  nss IN ("49924106823","31713900129","32584152022");

    DELETE  
    FROM   cta_ctr_cuenta
    WHERE  nss IN ("49924106823","31713900129","32584152022");

    LOAD FROM "cta_ctr_cuentaD.unl"  INSERT INTO cta_ctr_cuenta;
    LOAD FROM "cta_his_marcaD.unl"   INSERT INTO cta_his_marca 
    LOAD FROM "cta_act_marcaD.unl"   INSERT INTO cta_act_marca

    DELETE 
    FROM   ret_solicitud_rx
    WHERE  nss IN ("49924106823","31713900129","32584152022");

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP06-D"

    UPDATE ret_solicitud_tx
    SET  estado_solicitud   = 3 ,
         acciones_ret97     = 0 ,
         acciones_cuota_soc = 0 ,
         acciones_cv        = 0 ,
         acciones_ret92     = 0 ,
         saldo_viv97        = 0 ,
         saldo_viv92        = 0 ,
         rechazo_cod        = 0
    WHERE  tipo_prestacion  = 3
    AND    tipo_retiro      = "D"
    AND    nss IN ("49924106823","31713900129","32584152022")

    DELETE
    FROM   ret_solicitud_rx
    WHERE  folio IN(SELECT folio
                    FROM   ret_cza_lote
                    WHERE  nom_archivo = "OP06-D"
                   )

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro = "D"
    AND    estado      = 2

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro  = "D"
    AND    fecha_genera = TODAY

    DELETE
    FROM   dis_provision 
    WHERE  nss IN ("49924106823","31713900129","32584152022")

    LET cp = "cp OP06-D ",g_param_ret.ruta_rescate
    RUN cp

    LET ch = "chmod 777 ",g_param_ret.ruta_rescate CLIPPED,"/OP06-D"
    RUN ch
END FUNCTION



FUNCTION ret_total()
#rt-----------------
    DEFINE #loc #smallint
        i                     SMALLINT

    DELETE
    FROM   ret_consecutivo
    WHERE  consecutivo > 6

    FOR i = 1 TO 6
        INSERT INTO ret_consecutivo VALUES(i)
    END FOR

    DELETE
    FROM   glo_folio
    WHERE  folio > 6

    DELETE
    FROM   ret_resol_retiro
    WHERE  folio > 6

    DELETE
    FROM   ret_resol_retiro
    WHERE  n_seguro IN("72703311570","03633704626")

    LOAD FROM "resol_total_nagativa.unl" INSERT INTO ret_resol_retiro

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss in("72703311570","03633704626")

    LOAD FROM "ctr_cta_total_negativa.unl" INSERT INTO cta_ctr_cuenta

    DELETE
    FROM   dis_cuenta
    WHERE  nss in("72703311570","03633704626")
    AND    subcuenta = 4

    LOAD FROM "cuenta_total_negativa.unl" INSERT INTO dis_cuenta

    DELETE
    FROM   ret_seguimiento
    WHERE  folio > 6

    DELETE
    FROM   ret_ctr_envio
    WHERE  fecha_envio = HOY

    SELECT folio
    INTO   i_folio
    FROM   ret_cza_lote
    WHERE  nom_archivo = "RET001221.13NEG"

    DELETE 
    FROM   ret_cza_pe13
    WHERE  folio = i_folio

    DELETE
    FROM   ret_his_det_pe13
    WHERE  folio = i_folio

    DELETE
    FROM   ret_sum_pe13
    WHERE  folio = i_folio

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "RET001221.13NEG"

    DELETE
    FROM   ret_his_det_pe12
END FUNCTION


FUNCTION aclaraciones()
#a---------------------
         DELETE
	 FROM   ret_cza43

	 DELETE
	 FROM   ret_his_det_trab43

	 DELETE
	 FROM   ret_his_det_mov43

	 DELETE
	 FROM   ret_sum43

	 DELETE
	 FROM   ret_acl_icefa

	 DELETE
	 FROM   ret_acl_retiro

         DELETE
	 FROM   ret_resol_retiro
         WHERE  n_seguro IN("01633918675","24552901050","39885005460",
	    	            "45897059148","49742300616")

    LOAD FROM "resol_retiros_aclara.unl" INSERT INTO ret_resol_retiro


    DELETE
    FROM   ret_his_det_pe29
    WHERE  n_seguro IN("01633918675","24552901050","39885005460",
	   	       "45897059148","49742300616")

    LOAD FROM "his_det_pe_29_aclara.unl" INSERT INTO ret_his_det_pe29


    DELETE
    FROM   tra_det_trasp_sal

    LOAD FROM "det_trasp_sal_aclara.unl" INSERT INTO tra_det_trasp_sal

    DELETE
    FROM   tra_det_trasp_int

    LOAD FROM "det_trasp_int_aclara.unl" INSERT INTO tra_det_trasp_int

    DELETE
    FROM   dis_cuenta
    WHERE  nss IN("01633918675","24552901050","39885005460",
	  	  "45897059148","49742300616")

    LOAD FROM "cuenta_aclara.unl" INSERT INTO dis_cuenta;
END FUNCTION


FUNCTION voluntarias()
#v--------------------
    DELETE
    FROM  afi_mae_afiliado
    WHERE n_seguro = "01623934153"

    LOAD FROM "maeafili_voluntaria.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM  dis_cuenta
    WHERE nss       = "01623934153"
    AND   subcuenta = 3
   
    LOAD FROM "cuentas_voluntarias.unl" INSERT INTO dis_cuenta

    DELETE
    FROM   ret_cta_vol
    WHERE  n_seguro = "01623934153"

    DELETE
    FROM   tab_retiro

    LOAD FROM "tab_retiro.unl" INSERT INTO tab_retiro

    DELETE
    FROM   tab_pago

    LOAD FROM "tab_pago.unl" INSERT INTO tab_pago

    DELETE
    FROM   tab_identificacion

    LOAD FROM "tab_identificacion.unl" INSERT INTO tab_identificacion
END FUNCTION


FUNCTION ini_global()
#ig------------------
    DELETE
    FROM  ret_cza_lote

    DELETE
    FROM  ret_seguimiento

    DELETE
    FROM  ret_cza_pe03

    DELETE
    FROM  ret_his_det_pe03

    DELETE
    FROM  ret_sum_pe03

    DELETE
    FROM  ret_cza_pe05;

    DELETE
    FROM  ret_his_det_pe05;

    DELETE
    FROM  ret_sum_pe05;

    UPDATE cta_ctr_cuenta
    SET    estado_cuenta  = 1,
           estado_proceso = 0
    WHERE  nss in("01554101053","01564009866","01573653472");
 
    DELETE
    FROM   ret_cza_pe17

    DELETE
    FROM   ret_his_det_pe17

    DELETE
    FROM   ret_sum_pe17

    DELETE
    FROM   ret_cza_pe25

    DELETE
    FROM   ret_his_det_pe24

    DELETE
    FROM   ret_RETC008

    DELETE
    FROM   ret_his_det_pe25

    DELETE
    FROM   ret_sum_pe25

    DELETE
    FROM   ret_cza_pe28

    DELETE
    FROM   ret_his_det_pe28

    DELETE
    FROM   ret_his_det_pe29

    DELETE
    FROM   ret_sum_pe28

    DELETE
    FROM   ret_sum_envio

    DELETE
    FROM  ret_cza_pe36

    DELETE
    FROM  ret_his_det_pe36

    DELETE
    FROM  ret_sum_pe36

    DELETE
    FROM  ret_cza_pe39

    DELETE
    FROM  ret_his_det_pe39

    DELETE
    FROM  ret_his_det_pe40

    DELETE
    FROM  ret_sum_pe39

    DELETE
    FROM   ret_ctr_envio

    DELETE
    FROM   ret_resol_retiro

    DELETE
    FROM  glo_folio  

    DELETE
    FROM  ret_consecutivo

    DELETE
    FROM  afi_mae_afiliado
    WHERE n_seguro IN("04695322851","54917801448","54843810166")

    LOAD FROM "maeafili_ivrt.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM  dis_cuenta 
    WHERE nss IN("04695322851","54917801448","54843810166")
    AND   tipo_movimiento BETWEEN 401 AND 499

    DELETE
    FROM   dis_cuenta 
    WHERE  nss in("01554101053","01564009866","01573653472")

    LOAD FROM "cuenta_sar.unl" INSERT INTO dis_cuenta


    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss in ("04695322851","54843810166","54917801448")

    LOAD FROM "ctr_cuenta_ivrt.unl" INSERT INTO cta_ctr_cuenta

    DELETE 
    FROM   cta_ctr_cuenta
    WHERE  nss in("01554101053","01564009866","01573653472");

    LOAD FROM "ctr_cuenta_sar92.unl" INSERT INTO cta_ctr_cuenta;

    DELETE
    FROM   ret_status

    LOAD FROM "ret_status.unl" INSERT INTO ret_status
END FUNCTION


FUNCTION tipo_retiroJ()
   DELETE from dis_provision
   WHERE nss IN ("55663710022","55753310436")
 
   DELETE from ret_solicitud_rx
   WHERE nss IN ("55663710022","55753310436")

   DELETE from afi_mae_afiliado
   WHERE n_seguro IN ("55663710022","55753310436")

   LOAD FROM "afi_mae_afiliadoJ.unl"
   INSERT INTO afi_mae_afiliado

   DELETE from dis_det_aporte
   WHERE n_seguro IN ("55663710022","55753310436")

   LOAD FROM "dis_det_aporteJ.unl"
   INSERT INTO dis_det_aporte

   DELETE from dis_cuenta97
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta97J.unl"
   INSERT INTO dis_cuenta97

   DELETE from dis_cuenta98
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta98J.unl"
   INSERT INTO dis_cuenta98

   DELETE from dis_cuenta99
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta99J.unl"
   INSERT INTO dis_cuenta99

   DELETE from dis_cuenta00
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta00J.unl"
   INSERT INTO dis_cuenta00
  
   DELETE from dis_cuenta01
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta01J.unl"
   INSERT INTO dis_cuenta01

   DELETE from dis_cuenta02
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta02J.unl"
   INSERT INTO dis_cuenta02

   DELETE from dis_cuenta03
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta03J.unl"
   INSERT INTO dis_cuenta03

   DELETE from dis_cuenta
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuentaJ.unl"
   INSERT INTO dis_cuenta
 
   DELETE from ret_solicitud_tx
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "ret_solicitud_txJ.unl"
   INSERT INTO ret_solicitud_tx
END FUNCTION
 

FUNCTION tipo_retiroABC()
   DELETE from ret_ctr_envio_lote
   WHERE tipo_retiro IN ("A","B","C")

   DELETE from ret_det_datamart
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from ret_transf_tx
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from ret_transf_rx
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from ret_cza_lote
   WHERE  nom_archivo = "20040902.ABC"

   DELETE from cta_act_marca
   WHERE nss IN ("65644320072","05685005414","51825100541")
  
   DELETE from cta_his_marca
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from cta_ctr_cuenta
   WHERE nss IN ("65644320072","05685005414","51825100541")
 
   LOAD FROM "cta_ctr_cuentaABC.unl"
   INSERT INTO cta_ctr_cuenta

   DELETE from dis_provision
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from afi_mae_afiliado
   WHERE n_seguro IN ("65644320072","05685005414","51825100541")

   LOAD FROM "afi_mae_afiliadoABC.unl"
   INSERT INTO afi_mae_afiliado

   DELETE from dis_det_aporte
   WHERE n_seguro IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_det_aporteABC.unl"
   INSERT INTO dis_det_aporte

   DELETE from dis_cuenta97
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta97ABC.unl"
   INSERT INTO dis_cuenta97

   DELETE from dis_cuenta98
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta98ABC.unl"
   INSERT INTO dis_cuenta98

   DELETE from dis_cuenta99
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta99ABC.unl"
   INSERT INTO dis_cuenta99

   DELETE from dis_cuenta00
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta00ABC.unl"
   INSERT INTO dis_cuenta00
  
   DELETE from dis_cuenta01
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta01ABC.unl"
   INSERT INTO dis_cuenta01

   DELETE from dis_cuenta02
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta02ABC.unl"
   INSERT INTO dis_cuenta02

   DELETE from dis_cuenta03
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta03ABC.unl"
   INSERT INTO dis_cuenta03

   DELETE from dis_cuenta
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuentaABC.unl"
   INSERT INTO dis_cuenta
END FUNCTION

