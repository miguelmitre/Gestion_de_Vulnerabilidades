##############################################################################
#Proyecto            => SAFRE  ( MEXICO )                                    #
#Owner               => E.F.P.                                               #
#Programa  CTAB013   => ESTADO DE CUENTA DETALLE DE MOVIMIENTOS              #
#Sistema             => CTA                                                  #
#By                  => OMAR SANDOVAL                                        #
#Actualizacion       => ARMANDO RODRIGUEZ CASTROPAREDES                      #
#Fecha act.          => 28 marzo 2006                                        #
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_nss RECORD
        nss               LIKE cta_ctr_cuenta.nss,
        folio_envio       LIKE cta_nss_edo_cta.folio_envio,
        curp              LIKE afi_mae_afiliado.n_unico
    END RECORD

    DEFINE reg_nss1 RECORD
        nss               LIKE cta_ctr_cuenta.nss,
        folio_envio       LIKE cta_nss_edo_cta.folio_envio
    END RECORD

    DEFINE w_aux RECORD
        seguro            LIKE cta_ctr_cuenta.nss,
        codven            LIKE afi_mae_afiliado.codven,
        n_unico           LIKE afi_mae_afiliado.n_unico,
        n_folio           LIKE afi_mae_afiliado.n_folio,
        tipo_solicitud    LIKE afi_mae_afiliado.tipo_solicitud,
        paterno           LIKE afi_mae_afiliado.paterno,
        materno           LIKE afi_mae_afiliado.materno,
        nombres           LIKE afi_mae_afiliado.nombres,
        rfc               LIKE afi_mae_afiliado.n_rfc,
        infonavit         LIKE afi_mae_afiliado.ind_infonavit,
        fentcons          LIKE afi_mae_afiliado.fentcons,
        sucursal          LIKE afi_mae_afiliado.coduni_n1,
        callep            LIKE afi_domicilio.calle,
        numep             LIKE afi_domicilio.numero,
        deptop            LIKE afi_domicilio.depto,
        coloniap          LIKE afi_domicilio.colonia,
        delegap           LIKE afi_domicilio.delega,
        estadop           LIKE afi_domicilio.estado,
        codposp           LIKE afi_domicilio.codpos,
        ciudadp           LIKE afi_domicilio.ciudad,
        fonop             LIKE afi_telefono.telefono
    END RECORD

    DEFINE det_rcv RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_cv RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_cs RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_sar RECORD
        fecha                  DATE,
        desc_mov               CHAR(45),
        id_aportante           CHAR(30),
        m_pesos                DECIMAL(10,2),
        subcuenta              SMALLINT,
        siefore_cod            SMALLINT,
        tipo_char              CHAR(29)
    END RECORD

    DEFINE det_saris RECORD
        fecha                  DATE,
        desc_mov               CHAR(45),
        id_aportante           CHAR(30),
        m_pesos                DECIMAL(10,2),
        subcuenta              SMALLINT,
        siefore_cod            SMALLINT,
        --tipo_movimiento        SMALLINT,
        tipo_char              CHAR(29),
        folio                  INTEGER,
        folio_sua              CHAR(6)
    END RECORD

    DEFINE det_viv97 RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_viv92 RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_vivis RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_vol RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_ret RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_aho RECORD
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    DEFINE det_sie RECORD
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        m_pesos               DECIMAL(10,2)
    END RECORD

    DEFINE g_reg RECORD
       estado                 INTEGER,
       status                 SMALLINT,
       folio_ini              INTEGER,
       folio_top              INTEGER,
       fecha_ini              DATE,
       fecha_top              DATE
    END RECORD

    DEFINE g_reg2 RECORD
       tipo_informe           SMALLINT,
       fecha_informe          DATE
    END RECORD

    DEFINE #glo #char
        CZA_LOTE              CHAR(040),
        CZALOTE               CHAR(040),
        CZA_IND               CHAR(040),
        CZAIND                CHAR(040),

        SAR_92                CHAR(040),     --detalle sar
        SAR92                 CHAR(040),
        SUM_SAR_92            CHAR(040),
        SUMSAR92              CHAR(040),

        SAR_ISSSTE            CHAR(040),     --detalle issste
        SARISSSTE             CHAR(040),
        SUM_SAR_ISSSTE        CHAR(040),
        SUMSARISSSTE          CHAR(040),

        RCV                   CHAR(040),     --detalle rcv
        RCVV                  CHAR(040),
        SUM_RCV               CHAR(040),
        SUMRCV                CHAR(040),

        CV                    CHAR(040),     --detalle cesantia y vejes
        CVV                   CHAR(040),
        SUM_CV                CHAR(040),
        SUMCV                 CHAR(040),

        CS                    CHAR(040),     --detalle cuota social
        CSV                   CHAR(040),
        SUM_CS                CHAR(040),
        SUMCS                 CHAR(040),

        VIV_92                CHAR(040),     --detalle viv92
        VIV92                 CHAR(040),
        SUM_VIV_92            CHAR(040),
        SUMVIV92              CHAR(040),

        VIV97                 CHAR(040),     --detalle viv97
        VIV_97                CHAR(040),
        SUM_VIV_97            CHAR(040),
        SUMVIV97              CHAR(040),

        VIVIS                 CHAR(040),     --detalle vivis
        VIV_IS                CHAR(040),
        SUM_VIV_IS            CHAR(040),
        SUMVIVIS              CHAR(040),

        VOL                   CHAR(040),     --detalle vol
        VOLL                  CHAR(040),
        SUMVOL                CHAR(040),
        SUM_VOL               CHAR(040),

        RETIRO                CHAR(040),     --detalle retiro
        VRETIRO               CHAR(040),
        SUM_RETIRO            CHAR(040),
        SUMRETIRO             CHAR(040),

        AHORRO                CHAR(040),     --detalle ahorro
        VAHORRO               CHAR(040),
        SUM_AHORRO            CHAR(040),
        SUMAHORRO             CHAR(040),

        XSIEFORE              CHAR(040),     --detalle siefore
        VSIEFORE              CHAR(040),
        SUM_SIEFORE           CHAR(040),
        SUMSIEFORE            CHAR(040),

        SUM_DIS               CHAR(040),
        SUMDIS                CHAR(040),

        sed                   CHAR(180),
        ELI                   CHAR(060),
        ELI2                  CHAR(060),
        ELI5                  CHAR(060),
        G_LISTA               CHAR(140),
        enter                 CHAR(001),
        cat                   CHAR(2000)

    DEFINE s_folio            SMALLINT

    DEFINE i_total_reg_lote   INTEGER,
           i_cont_de_nss      INTEGER,
           i_cont_de_reg      INTEGER,
           i_total_registros  INTEGER

   DEFINE g_parametro        RECORD LIKE seg_modulo.*
   DEFINE g_afore            RECORD LIKE tab_afore_local.*

   DEFINE razon_social	     CHAR(40)
   DEFINE vafore_receptora   CHAR(03)
   DEFINE af_domicilio       CHAR(40)
   DEFINE af_colonia         CHAR(40)
   DEFINE af_telefono        CHAR(40)
   DEFINE af_clave           CHAR(40)
   DEFINE ue_domicilio       CHAR(40)
   DEFINE ue_colonia         CHAR(40)
   DEFINE ue_telefono        CHAR(40)
   DEFINE ue_horario         CHAR(40)
   DEFINE ue_tel_grat        CHAR(40)

   DEFINE vtodo              DECIMAL(16,6)
   DEFINE HOY                DATE
   DEFINE hoy1               DATE
   DEFINE AYER               DATE
   DEFINE f_folio            DATE
   DEFINE hora               CHAR(8)
   DEFINE g_usuario          CHAR(8)
   DEFINE vestado_envio      CHAR(1)
   DEFINE vcentro_reparto    CHAR(5)
   DEFINE mes                SMALLINT
   DEFINE ano                SMALLINT
   DEFINE dias_del_mes       SMALLINT
   DEFINE i                  INTEGER
   DEFINE folio_cta          INTEGER

   DEFINE saldoira           DECIMAL(10,2)
   DEFINE saldoiraa          DECIMAL(10,2)
   DEFINE saldofra           DECIMAL(10,2)
   DEFINE saldofsi           DECIMAL(10,2)
   DEFINE saldofre           DECIMAL(10,2)
   DEFINE saldofcs           DECIMAL(10,2)
   DEFINE saldoicv           DECIMAL(10,2)
   DEFINE saldoicva          DECIMAL(10,2)
   DEFINE saldofcv           DECIMAL(10,2)
   DEFINE saldoivo           DECIMAL(10,2)
   DEFINE saldoivoa          DECIMAL(10,2)
   DEFINE saldofvo           DECIMAL(10,2)
   DEFINE saldofcr           DECIMAL(10,2)
   DEFINE saldofah           DECIMAL(10,2)
   DEFINE saldoiva           DECIMAL(10,2)
   DEFINE saldoivaa          DECIMAL(10,2)
   DEFINE saldofva           DECIMAL(10,2)
   DEFINE saldoivi           DECIMAL(10,2)
   DEFINE saldoivia          DECIMAL(10,2)
   DEFINE saldofvi           DECIMAL(10,2)
   DEFINE saldoisa           DECIMAL(10,2)
   DEFINE saldofsa           DECIMAL(10,2)
   DEFINE saldofvs           DECIMAL(10,2)
   DEFINE saldotvi           DECIMAL(10,2)
   DEFINE saldotvf           DECIMAL(10,2)

   DEFINE saldorrcv          DECIMAL(16,2)
   DEFINE saldoarcv          DECIMAL(16,2)
   DEFINE saldoprcv          DECIMAL(16,2)
   DEFINE saldorviv          DECIMAL(16,2)
   DEFINE saldoaviv          DECIMAL(16,2)
   DEFINE saldopviv          DECIMAL(16,2)
   DEFINE saldortot          DECIMAL(16,2)
   DEFINE saldoatot          DECIMAL(16,2)
   DEFINE saldoptot          DECIMAL(16,2)
   DEFINE saldoitot          DECIMAL(16,2)
   DEFINE saldoftot          DECIMAL(16,2)

   DEFINE comircv            DECIMAL(16,2)
   DEFINE comivol            DECIMAL(16,2)
   DEFINE comisal            DECIMAL(16,2)
   DEFINE comitod            DECIMAL(16,2)

   DEFINE numeaccin          DECIMAL(16,6)
   DEFINE precioini          DECIMAL(16,6)
   DEFINE preciofin          DECIMAL(16,6)
   DEFINE numeaccfin         DECIMAL(16,6)
   DEFINE importe1           DECIMAL(11,2)
   DEFINE importe2           DECIMAL(11,2)
   DEFINE importe3           DECIMAL(11,2)
   DEFINE importe4           DECIMAL(11,2)
   DEFINE porcenta           DECIMAL(5,2)
   DEFINE rendimiento        DECIMAL(5,2)

   DEFINE precioant          DECIMAL(16,6)
   DEFINE precioact          DECIMAL(16,6)

   DEFINE vabonos_sar        DECIMAL(16,2)
   DEFINE vcargos_sar        DECIMAL(16,2)
   DEFINE vabonos_rcv        DECIMAL(16,2)
   DEFINE vcargos_rcv        DECIMAL(16,2)
   DEFINE vabonos_viv92      DECIMAL(16,2)
   DEFINE vcargos_viv92      DECIMAL(16,2)
   DEFINE vabonos_viv97      DECIMAL(16,2)
   DEFINE vcargos_viv97      DECIMAL(16,2)
   DEFINE vabonos_vivis      DECIMAL(16,2)
   DEFINE vcargos_vivis      DECIMAL(16,2)
   DEFINE vabonos_vol        DECIMAL(16,2)
   DEFINE vcargos_vol        DECIMAL(16,2)
   DEFINE vcargos_sie        DECIMAL(16,2)
   DEFINE vconsecutivo       INTEGER

   DEFINE salapert           DECIMAL(16,2)
   DEFINE estad              CHAR(20)
   DEFINE delega             CHAR(40)

   DEFINE fechaini           DATE
   DEFINE fechafin           DATE
   DEFINE vfecha_ini         DATE
   DEFINE vfecha_top         DATE
   DEFINE vfecha_valor       DATE
   DEFINE vfecha_valor92     DATE
   DEFINE vfemision_tra      DATE

   DEFINE vrfc               CHAR(13)
   DEFINE vdesc_afore        CHAR(30)
   DEFINE vtasa_oper         DECIMAL(8,6)
   DEFINE vflag              SMALLINT
   DEFINE vperiodo_pago      CHAR(06)
   DEFINE vbimestre          CHAR(06)
   DEFINE vfolio_sua         CHAR(06)
   DEFINE vperiodo_int       CHAR(07)

   DEFINE f_hoy              DATE
   DEFINE dia                SMALLINT
   DEFINE vestado_cuenta     SMALLINT

   DEFINE reg_bat     RECORD
          pid                INTEGER,
          proceso_cod        INTEGER,
          opera_cod          INTEGER
   END RECORD

   DEFINE txt_dom            CHAR(2000)
   DEFINE txt_tel            CHAR(1000)
   DEFINE l_existe           SMALLINT
   DEFINE l_existe1          SMALLINT

   DEFINE bandera          SMALLINT,
          ruta_archivo     CHAR(300)

   DEFINE x_tipo_informe   SMALLINT,
          x_folio          INTEGER,
          pos              SMALLINT

   DEFINE tot_sie01_rcv     ,
          tot_sie01_vol     ,
          tot_sie02_rcv     ,
          tot_sie02_vol     ,
          tot_sie03_rcv     ,
          tot_sie03_vol     DECIMAL(10,2)

   DEFINE saldof1re   ,
          saldof1cv   ,
          saldof1cs   ,
          saldof1ra   ,
          saldof1si   ,
          saldof1vo   ,
          saldof1cr   ,
          saldof1ah   ,
          saldof2vo   ,
          saldof2re   ,
          saldof2cv   ,
          saldof2cs   ,
          saldof2ra   ,
          saldof2si   ,
          saldof2cr   ,
          saldof2ah   ,
          saldof3re   ,
          saldof3cv   ,
          saldof3cs   ,
          saldof3ra   ,
          saldof3si   ,
          saldof3vo   ,
          saldof3cr   ,
          saldof3ah   DECIMAL(10,2)

   DEFINE x_abono_rcv    ,
          x_cargo_rcv    ,
          x_abono_cv     ,
          x_cargo_cv     ,
          x_abono_cs     ,
          x_cargo_cs     ,
          x_abono_sar    ,
          x_cargo_sar    ,
          x_abono_saris  ,
          x_cargo_saris  ,

          x_abono_viv92  ,
          x_cargo_viv92  ,
          x_abono_viv97  ,
          x_cargo_viv97  ,
          x_abono_vivis  ,
          x_cargo_vivis  ,

          x_abono_vol    ,
          x_cargo_vol    ,
          x_abono_aho    ,
          x_cargo_aho    ,
          x_abono_ret    ,
          x_cargo_ret    DECIMAL(10,2)

   DEFINE total_cargo_01 ,
          total_abono_01 ,
          total_final_01 DECIMAL(10,2)

   DEFINE total_cargo_02 ,
          total_abono_02 ,
          total_final_02 DECIMAL(10,2)

   DEFINE total_cargo_03 ,
          total_abono_03 ,
          total_final_03 DECIMAL(10,2)

   DEFINE reg_sie ARRAY[100] OF RECORD
      cod_sie       SMALLINT,
      desc_sie      CHAR(50)
   END RECORD

   DEFINE nom_sie1   ,
          nom_sie2   ,
          nom_sie3   CHAR(04)

   DEFINE k  SMALLINT

END GLOBALS
############################################################
#FUNCTION CTAB013(valor1,valor2)
MAIN

   DEFINE valor1    CHAR(11),
          valor2    CHAR(19),
          valor3    INTEGER,
          valor4    CHAR(19)

   LET valor1 = ARG_VAL(1)
   LET valor2 = ARG_VAL(2)
   LET valor3 = ARG_VAL(3)

   LET reg_nss.nss  = valor1

   IF valor1 IS NOT NULL THEN
      LET x_folio  = valor2
   ELSE
      LET reg_nss.curp = valor2
      LET x_folio      = valor3
   END IF

   DISPLAY "INICIA PROCESO DE DETALLE DE MOVIMIENTOS CON FOLIO: ",x_folio

   IF reg_nss.nss IS NULL AND
      (reg_nss.curp IS NOT NULL OR reg_nss.curp = " ") THEN
         SELECT n_seguro
         INTO   reg_nss.nss
         FROM   afi_mae_afiliado
         WHERE  n_unico = reg_nss.curp
   END IF

   CALL STARTLOG("CTAB013.log")
   CALL inicializa()
   CALL obtiene_emision()

   #RETURN bandera, ruta_archivo

   DISPLAY "TERMINA PROCESO DE DETALLE DE MOVIMIENTOS DEL NSS: ",reg_nss.nss

END MAIN
############################################################
FUNCTION inicializa()
#i-------------------

   UPDATE cta_ctr_proceso
   SET    cta_ctr_proceso.estado = 2
   WHERE  cta_ctr_proceso.folio = x_folio
   AND    cta_ctr_proceso.estado = 1

   LET HOY  = TODAY
   LET hoy1 = HOY - 1 UNITS DAY
   LET hora = TIME

   LET f_hoy            = HOY
   LET i_total_reg_lote = 0
   LET vfemision_tra    = HOY

   LET saldofra = 0
   LET saldofsi = 0
   LET saldofcv = 0
   LET saldofre = 0
   LET saldofcs = 0
   LET saldofvo = 0
   LET saldofvi = 0
   LET saldofcr = 0
   LET saldofah = 0
   LET saldofva = 0
   LET saldofvs = 0

   LET saldof1re = 0
   LET saldof1cv = 0
   LET saldof1cs = 0
   LET saldof1ra = 0
   LET saldof1si = 0
   LET saldof1vo = 0
   LET saldof1cr = 0
   LET saldof1ah = 0
   LET saldof2vo = 0
   LET saldof2re = 0
   LET saldof2cv = 0
   LET saldof2cs = 0
   LET saldof2ra = 0
   LET saldof2si = 0
   LET saldof2cr = 0
   LET saldof2ah = 0
   LET saldof3re = 0
   LET saldof3cv = 0
   LET saldof3cs = 0
   LET saldof3ra = 0
   LET saldof3si = 0
   LET saldof3vo = 0
   LET saldof3cr = 0
   LET saldof3ah = 0

   LET x_abono_rcv    = 0
   LET x_cargo_rcv    = 0
   LET x_abono_cv     = 0
   LET x_cargo_cv     = 0
   LET x_abono_cs     = 0
   LET x_cargo_cs     = 0
   LET x_abono_sar    = 0
   LET x_cargo_sar    = 0
   LET x_abono_saris  = 0
   LET x_cargo_saris  = 0

   LET x_abono_viv92  = 0
   LET x_cargo_viv92  = 0
   LET x_abono_viv97  = 0
   LET x_cargo_viv97  = 0
   LET x_abono_vivis  = 0
   LET x_cargo_vivis  = 0

   LET x_abono_vol    = 0
   LET x_cargo_vol    = 0
   LET x_abono_aho    = 0
   LET x_cargo_aho    = 0
   LET x_abono_ret    = 0
   LET x_cargo_ret    = 0

   LET total_cargo_01 = 0
   LET total_abono_01 = 0
   LET total_final_01 = 0

   LET total_cargo_02 = 0
   LET total_abono_02 = 0
   LET total_final_02 = 0

   LET total_cargo_03 = 0
   LET total_abono_03 = 0
   LET total_final_03 = 0

   SELECT *,USER
   INTO   g_afore.*,g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_parametro.*
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET AYER = habil_anterior(hoy1)

   CALL datos_trabajador()
   CALL saldo_al_dia()
   CALL datos_afore()

END FUNCTION #inicializa
############################################################
FUNCTION obtiene_emision()

   CALL nuevo_folio()      #nf
   CALL genera_patrones()  #gp
   CALL pide_parametros()  #pp
   CALL prepara_nss()      #pn

END FUNCTION #obtiene_emision
############################################################
FUNCTION nuevo_folio()

   SELECT MAX(consecutivo_dia)
   INTO   s_folio
   FROM   cta_ctr_det_mov
   WHERE  factualiza = HOY

   IF s_folio IS NULL THEN
      LET s_folio = 1
   ELSE
      LET s_folio = s_folio + 1
   END IF

   LET f_folio = HOY

   INSERT INTO cta_ctr_det_mov
   VALUES(reg_nss.nss,
          s_folio,
          HOY,
          USER)

   LET ELI  = g_parametro.ruta_envio CLIPPED,"/"
   LET ELI5 = g_parametro.ruta_envio CLIPPED,"/","DM-",HOY USING "MMDD","-",
              reg_nss.nss CLIPPED,".",s_folio USING "&&&" CLIPPED

   LET ELI2 = HOY USING"DDMM","_",s_folio USING "&&&" CLIPPED
   LET ruta_archivo = ELI5 CLIPPED

END FUNCTION
############################################################
FUNCTION pide_parametros()

   DEFINE dia   SMALLINT

   LET vfecha_top = TODAY
   LET dia = DAY(vfecha_top)
{
   IF MONTH(vfecha_top) > 1 THEN
      LET vfecha_ini = MDY(1,1,YEAR(TODAY))
   ELSE
      LET vfecha_ini = vfecha_top - dia UNITS DAY
      LET vfecha_ini = vfecha_ini + 1 UNITS DAY
      LET vfecha_ini = vfecha_ini - 6 UNITS MONTH
   END IF
}
END FUNCTION #pide_parametros
############################################################
FUNCTION prepara_nss()

   SELECT a.tipo_informe
   INTO   x_tipo_informe
   FROM   cta_ctr_proceso a
   WHERE  a.folio = x_folio
   AND    a.estado = 2
   GROUP BY 1

   CALL Ingresa_etapa(x_folio,2,0,"Inicia calculo de detalle de movimientos")
   CALL estado_cuenta()

   LET cat = "rm ",ELI CLIPPED,"CZA*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"SUM*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"RCV*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"CV*"    RUN cat
   LET cat = "rm ",ELI CLIPPED,"CS*"    RUN cat
   LET cat = "rm ",ELI CLIPPED,"VIV*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"VOL*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"SAR*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"VRE*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"VAH*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"AHO*"   RUN cat
   LET cat = "rm ",ELI CLIPPED,"RET*"   RUN cat

   UPDATE cta_ctr_proceso
   SET    cta_ctr_proceso.estado = 3,
          cta_ctr_proceso.fecha_fin = TODAY
   WHERE  cta_ctr_proceso.folio  = x_folio
   AND    cta_ctr_proceso.estado = 2

   CALL Actualiza_etapa(x_folio,2,pos,"Termina calculo detalle de movimientos")
   CALL Actualiza_etapa(x_folio,1,pos,ELI5)

END FUNCTION
############################################################
FUNCTION estado_cuenta()
#---------------------------------  Detalle Estado de Cuenta
   DEFINE permisos      CHAR(100)

   LET CZA_IND          = ELI CLIPPED,"CZA_IND",ELI2
   LET CZAIND           = ELI CLIPPED,"CZAIND",ELI2

   LET SAR_92           = ELI CLIPPED,"SAR_92",ELI2
   LET SAR92            = ELI CLIPPED,"SAR92",ELI2
   LET SUM_SAR_92       = ELI CLIPPED,"SUM_SAR_92",ELI2
   LET SUMSAR92         = ELI CLIPPED,"SUMSAR92",ELI2

   LET SAR_ISSSTE       = ELI CLIPPED,"SAR_ISSSTE",ELI2
   LET SARISSSTE        = ELI CLIPPED,"SARISSSTE",ELI2
   LET SUM_SAR_ISSSTE   = ELI CLIPPED,"SUM_SAR_ISSSTE",ELI2
   LET SUMSARISSSTE     = ELI CLIPPED,"SUMSARISSSTE",ELI2

   LET RCV              = ELI CLIPPED,"RCV",ELI2
   LET RCVV             = ELI CLIPPED,"RCVV",ELI2
   LET SUM_RCV          = ELI CLIPPED,"SUM_RCV",ELI2
   LET SUMRCV           = ELI CLIPPED,"SUMRCV",ELI2

   LET CV               = ELI CLIPPED,"CV",ELI2
   LET CVV              = ELI CLIPPED,"CVV",ELI2
   LET SUM_CV           = ELI CLIPPED,"SUM_CV",ELI2
   LET SUMCV            = ELI CLIPPED,"SUMCV",ELI2

   LET CS               = ELI CLIPPED,"CS",ELI2
   LET CSV              = ELI CLIPPED,"CSV",ELI2
   LET SUM_CS           = ELI CLIPPED,"SUM_CS",ELI2
   LET SUMCS            = ELI CLIPPED,"SUMCS",ELI2

   LET VIV_92           = ELI CLIPPED,"VIV_92",ELI2
   LET VIV92            = ELI CLIPPED,"VIV92",ELI2
   LET SUM_VIV_92       = ELI CLIPPED,"SUM_VIV_92",ELI2
   LET SUMVIV92         = ELI CLIPPED,"SUMVIV92",ELI2

   LET VIV_97           = ELI CLIPPED,"VIV_97",ELI2
   LET VIV97            = ELI CLIPPED,"VIV97",ELI2
   LET SUM_VIV_97       = ELI CLIPPED,"SUM_VIV_97",ELI2
   LET SUMVIV97         = ELI CLIPPED,"SUMVIV97",ELI2

   LET VIV_IS           = ELI CLIPPED,"VIV_IS",ELI2
   LET VIVIS            = ELI CLIPPED,"VIVIS",ELI2
   LET SUM_VIV_IS       = ELI CLIPPED,"SUM_VIV_IS",ELI2
   LET SUMVIVIS         = ELI CLIPPED,"SUMVIVIS",ELI2

   LET VOL              = ELI CLIPPED,"VOL",ELI2
   LET VOLL             = ELI CLIPPED,"VOLL",ELI2
   LET SUM_VOL          = ELI CLIPPED,"SUM_VOL",ELI2
   LET SUMVOL           = ELI CLIPPED,"SUMVOL",ELI2

   LET RETIRO           = ELI CLIPPED,"RETIRO",ELI2
   LET VRETIRO          = ELI CLIPPED,"VRETIRO",ELI2
   LET SUM_RETIRO       = ELI CLIPPED,"SUM_RETIRO",ELI2
   LET SUMRETIRO        = ELI CLIPPED,"SUMRETIRO",ELI2

   LET AHORRO           = ELI CLIPPED,"AHORRO",ELI2
   LET VAHORRO          = ELI CLIPPED,"VAHORRO",ELI2
   LET SUM_AHORRO       = ELI CLIPPED,"SUM_AHORRO",ELI2
   LET SUMAHORRO        = ELI CLIPPED,"SUMAHORRO",ELI2

   LET XSIEFORE         = ELI CLIPPED,"XSIEFORE",ELI2
   LET VSIEFORE         = ELI CLIPPED,"VSIEFORE",ELI2
   LET SUM_SIEFORE      = ELI CLIPPED,"SUM_SIEFORE",ELI2
   LET SUMSIEFORE       = ELI CLIPPED,"SUMSIEFORE",ELI2

   LET SUM_DIS          = ELI CLIPPED,"SUM_DIS",ELI2
   LET SUMDIS           = ELI CLIPPED,"SUMDIS",ELI2

   CALL cza_individual()          --detalle del encabezado
   CALL detalle_rcv()	          --detalle de rcv
   CALL detalle_cv()	          --detalle de cesantia y vejes
   CALL detalle_cs()	          --detalle de cuota social
   CALL detalle_sar()	          --detalle de sar imss
   CALL detalle_sar_issste()      --detalle de sar issste

   CALL detalle_vol()	          --detalle aportaciones voluntarias
   CALL detalle_retiro()          --detalle aportaciones complementarias
   CALL detalle_ahorro()          --detalle ahorro largo plazo

   CALL detalle_viv()             --detalle de vivienda
   CALL calcula_totales()         --calcula total cargos y abonos
   CALL sumario()                 --detalle sumario total

   LET i_total_registros = 0
   WHENEVER ERROR CONTINUE
      LET cat ="cat ",CZAIND   ," ",
                      SAR92    ," ",SUMSAR92    ," ",
                      SARISSSTE," ",SUMSARISSSTE," ",
                      RCVV     ," ",SUMRCV      ," ",
                      CVV      ," ",SUMCV       ," ",
                      CSV      ," ",SUMCS       ," ",
                      VIV92    ," ",SUMVIV92    ," ",
                      VIV97    ," ",SUMVIV97    ," ",
                      VIVIS    ," ",SUMVIVIS    ," ",
                      VOLL     ," ",SUMVOL      ," ",
                      VRETIRO  ," ",SUMRETIRO   ," ",
                      VAHORRO  ," ",SUMAHORRO   ," ",
                      SUMDIS   ," >> ",ELI5 CLIPPED
      RUN cat

      IF SQLCA.SQLCODE = 0 THEN
         LET bandera = 1
      ELSE
         LET bandera = 0
      END IF

      LET permisos = "chmod 777 ",ELI CLIPPED,"DM-*" CLIPPED
      RUN permisos

   WHENEVER ERROR STOP

END FUNCTION #estado_cuenta
############################################################
FUNCTION genera_patrones()
#gp-----------------------

   DEFINE rfc RECORD
       reg_patronal_imss  CHAR(11),
       rfc_patron         CHAR(13),
       consec_reg_lote    INTEGER,
       folio_sua          INTEGER
   END RECORD

   WHENEVER ERROR CONTINUE
      SET LOCK MODE TO WAIT
          DROP TABLE temp_rfc_patron01
          CREATE TEMP TABLE temp_rfc_patron01
                (reg_patronal_imss CHAR(11),
                 rfc_patron        CHAR(13),
                 consec_reg_lote   INTEGER,
                 folio_sua         INTEGER)
      SET LOCK MODE TO NOT WAIT

      SET LOCK MODE TO WAIT
          DROP TABLE tmp_cuenta
          CREATE TEMP TABLE tmp_cuenta
          ( tipo_movimiento   SMALLINT NOT NULL,
            subcuenta         SMALLINT NOT NULL,
            siefore           SMALLINT,
            folio             INTEGER  NOT NULL,
            consecutivo_lote  INTEGER,
            nss               CHAR(11) NOT NULL,
            curp              CHAR(18),
            folio_sua         CHAR(6),
            fecha_pago        DATE,
            fecha_valor       DATE,
            fecha_conversion  DATE,
            monto_en_pesos    DECIMAL(22,6),
            monto_en_acciones DECIMAL(22,6),
            precio_accion     DECIMAL(22,6),
            dias_cotizados    SMALLINT,
            sucursal          CHAR(10),
            id_aportante      CHAR(11),
            estado            SMALLINT,
            fecha_proceso     DATE,
            usuario           CHAR(8),
            fecha_archivo     DATE,
            etiqueta          SMALLINT )
      SET LOCK MODE TO NOT WAIT
   WHENEVER ERROR STOP

   INSERT INTO temp_rfc_patron01
   SELECT reg_patronal_imss,
          rfc_patron,
          consec_reg_lote,
          folio_pago_sua
   FROM   dis_det_aporte
   WHERE  n_seguro = reg_nss.nss

   INSERT INTO tmp_cuenta
   SELECT *
   FROM   dis_cuenta
   WHERE  nss = reg_nss.nss
   AND    tipo_movimiento <> 999

   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta (subcuenta,id_aportante)

   DECLARE cur_pat CURSOR FOR
   SELECT *
   FROM   temp_rfc_patron01

   FOREACH cur_pat INTO rfc.*
     IF rfc.rfc_patron[1,1] MATCHES " " THEN
         SET LOCK MODE TO WAIT
           UPDATE temp_rfc_patron01
           SET    rfc_patron        = rfc_patron[2,13]
           WHERE  reg_patronal_imss = rfc.reg_patronal_imss
	   AND    consec_reg_lote   = rfc.consec_reg_lote
	   AND    folio_sua         = rfc.folio_sua
         SET LOCK MODE TO NOT WAIT
    	 END IF
   END FOREACH
END FUNCTION #genera_patrones
############################################################
FUNCTION datos_trabajador()
#dt------------------------
    DEFINE finfonavit    DATE
    DEFINE finfonavit_1  DATE
    DEFINE codigo_postal char(5)

    LET finfonavit   = ""
    LET finfonavit_1 = ""

    SELECT a.n_seguro,
           a.codven,
           a.n_unico,
           a.n_folio,
           a.tipo_solicitud,
           a.paterno,
           a.materno,
           a.nombres,
           a.n_rfc,
           a.ind_infonavit,
           a.fentcons,
           a.coduni_n1,
           "",   --#a.calle,
           "",   --#a.nume,
           "",   --#a.deptop,
           "",   --#a.coloniap,
           "",   --#a.delegap,
           "",   --#a.estadop,
           "",   --#a.codposp,
           "",   --#a.ciudadp,
           "",   --#a.fonop,
           "",
           "",
           "",
           "",
           "",
           ""
    INTO   w_aux.*
    FROM   afi_mae_afiliado a
    WHERE  a.n_seguro = reg_nss.nss

    LET w_aux.infonavit = "0"

    SELECT "X"
    FROM   acr_det_cedido
    WHERE  nss_afore = reg_nss.nss
    AND    estado    = 0
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
       SELECT MAX(fecha_trasp)
       INTO   finfonavit
       FROM   acr_det_cedido
       WHERE  nss_afore = reg_nss.nss
       AND    estado    = 0

	     SELECT "X"
	     FROM   acr_det_devuelto
	     WHERE  n_seguro    = reg_nss.nss
	     AND   estado in(1,0)
	     GROUP BY 1

       IF STATUS <> NOTFOUND THEN
	        SELECT MAX(fecha_mov_banxico)
	        INTO   finfonavit_1
	        FROM   acr_det_devuelto
	        WHERE  n_seguro    =  reg_nss.nss
	        AND   estado = 0

          IF finfonavit < finfonavit_1 THEN
             LET w_aux.infonavit = "0"
          ELSE
             LET w_aux.infonavit = "1"
          END IF
       ELSE
          LET w_aux.infonavit = "1"
       END IF
    ELSE
        LET w_aux.infonavit = "0"
    END IF

    SELECT "X"
    FROM   acr_det_tra_cred
    WHERE  nss_afore = reg_nss.nss
    AND    estado in(1,0)
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
	     LET w_aux.infonavit = "2"
    END IF

    LET txt_tel = " SELECT FIRST 1 a.tel_cod ",
                  " FROM  afi_telefono a ",
                  " WHERE a.n_folio = ",w_aux.n_folio CLIPPED,
                  " AND   a.tipo_solicitud = ",w_aux.tipo_solicitud CLIPPED

    INITIALIZE w_aux.fonop TO NULL

    PREPARE p_tel FROM txt_tel
    DECLARE c_tel CURSOR FOR p_tel

    FOREACH c_tel INTO   w_aux.fonop
       LET l_existe1 = TRUE
    END FOREACH
    FREE c_tel

    LET  vestado_cuenta = 0

    LET txt_dom = "SELECT FIRST 1 a.calle,",
                                " a.numero,",
                                " a.depto,",
                                " a.colonia,",
                                " a.delega,",
                                " a.estado,",
                                " a.codpos,",
                                " a.ciudad ",
                  " FROM  afi_domicilio a ",
                  " WHERE a.nss = ","'",reg_nss.nss CLIPPED,"'",
                  " AND   a.n_folio = ",w_aux.n_folio CLIPPED,
                  " AND   a.tipo_solicitud = ",w_aux.tipo_solicitud CLIPPED

   PREPARE p_dom FROM txt_dom
   DECLARE c_dom CURSOR FOR p_dom

   INITIALIZE w_aux.callep   TO NULL
   INITIALIZE w_aux.numep    TO NULL
   INITIALIZE w_aux.deptop   TO NULL
   INITIALIZE w_aux.coloniap TO NULL
   INITIALIZE w_aux.delegap  TO NULL
   INITIALIZE w_aux.estadop  TO NULL
   INITIALIZE w_aux.codposp  TO NULL
   INITIALIZE w_aux.ciudadp  TO NULL

   FOREACH c_dom INTO w_aux.callep,
                      w_aux.numep,
                      w_aux.deptop,
                      w_aux.coloniap,
                      w_aux.delegap,
                      w_aux.estadop,
                      w_aux.codposp,
                      w_aux.ciudadp
      LET l_existe = TRUE
   END FOREACH
   FREE c_dom

   LET estad  = " "
   LET delega = " "
   LET codigo_postal = w_aux.codposp

   SELECT estad_desc
   INTO   estad
   FROM   tab_estado
   WHERE  estad_cod = w_aux.estadop

   SELECT deleg_desc
   INTO   delega
   FROM   tab_delegacion
   WHERE  deleg_cod = w_aux.delegap

   SELECT c.centro_reparto
   INTO   vcentro_reparto
   FROM   tab_reparto c
   WHERE  c.codigo_postal = w_aux.codposp

   LET vafore_receptora = " "
   LET vestado_envio = "1"

END FUNCTION #datos_trabajador
############################################################
FUNCTION saldo_al_dia()
#sd--------------------

   DEFINE v_saldo_dia        CHAR(100),
          v_nss              CHAR(11),
          v_subcuenta        SMALLINT,
          v_grupo            SMALLINT

   DEFINE f_subcuenta        SMALLINT,
          f_siefore          SMALLINT,
          f_monto_acc        DECIMAL(16,6),
          f_monto_pes        DECIMAL(10,2)

   DEFINE v_saldo_acc        DECIMAL(16,6),
          v_saldo_pes        DECIMAL(16,6)

   CREATE TEMP TABLE tmp_saldo_dia
    (subcuenta       SMALLINT,
     siefore         SMALLINT,
     acciones        DECIMAL(16,6),
     pesos           DECIMAL(16,6)
    )

   LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
   PREPARE eje_saldo_dia FROM v_saldo_dia

   LET v_nss       = reg_nss.nss
   LET v_subcuenta = 0
   LET v_grupo     = 0
   LET v_saldo_acc = 0
   LET v_saldo_pes = 0

   DECLARE c_saldo CURSOR FOR  eje_saldo_dia
   FOREACH c_saldo  USING v_nss,
                          v_subcuenta,
                          v_grupo,
                          HOY
                     INTO f_subcuenta,
                          f_siefore,
                          f_monto_acc,
                          f_monto_pes

      IF f_monto_pes IS NULL OR
         f_monto_pes = " "   THEN
            LET f_monto_pes = 0
      END IF

      INSERT INTO tmp_saldo_dia
      VALUES(f_subcuenta,
             f_siefore,
             f_monto_acc,
             f_monto_pes)

   END FOREACH

   DECLARE tmp_sal CURSOR FOR
   SELECT subcuenta,
          siefore,
          ROUND(pesos,2)
   FROM   tmp_saldo_dia
   ORDER BY 1,2

   FOREACH tmp_sal INTO f_subcuenta,
                        f_siefore,
                        f_monto_pes

   CASE f_siefore
      WHEN 1
         CASE f_subcuenta
--ahorro retiro
            WHEN 1
                 LET saldof1re = saldof1re + f_monto_pes
            WHEN 2
                 LET saldof1cv = saldof1cv + f_monto_pes
            WHEN 6
                 LET saldof1cv = saldof1cv + f_monto_pes
            WHEN 9
                 LET saldof1cv = saldof1cv + f_monto_pes
            WHEN 5
                 LET saldof1cs = saldof1cs + f_monto_pes
            WHEN 7
                 LET saldof1ra = saldof1ra + f_monto_pes
            WHEN 13
                 LET saldof1si = saldof1si + f_monto_pes
--ahorro voluntario
            WHEN 3
                 LET saldof1vo = saldof1vo + f_monto_pes
            WHEN 10
                 LET saldof1vo = saldof1vo + f_monto_pes
            WHEN 11
                 LET saldof1cr = saldof1cr + f_monto_pes
            WHEN 12
                 LET saldof1cr = saldof1cr + f_monto_pes
            WHEN 15
                 LET saldof1ah = saldof1ah + f_monto_pes
            WHEN 16
                 LET saldof1ah = saldof1ah + f_monto_pes
         END CASE

      WHEN 2
         CASE f_subcuenta
--ahorro retiro
            WHEN 1
                 LET saldof2re = saldof2re + f_monto_pes
            WHEN 2
                 LET saldof2cv = saldof2cv + f_monto_pes
            WHEN 6
                 LET saldof2cv = saldof2cv + f_monto_pes
            WHEN 9
                 LET saldof2cv = saldof2cv + f_monto_pes
            WHEN 5
                 LET saldof2cs = saldof2cs + f_monto_pes
            WHEN 7
                 LET saldof2ra = saldof2ra + f_monto_pes
            WHEN 13
                 LET saldof2si = saldof2si + f_monto_pes
--ahorro voluntario
            WHEN 3
                 LET saldof2vo = saldof2vo + f_monto_pes
            WHEN 10
                 LET saldof2vo = saldof2vo + f_monto_pes
            WHEN 11
                 LET saldof2cr = saldof2cr + f_monto_pes
            WHEN 12
                 LET saldof2cr = saldof2cr + f_monto_pes
            WHEN 15
                 LET saldof2ah = saldof2ah + f_monto_pes
            WHEN 16
                 LET saldof2ah = saldof2ah + f_monto_pes
         END CASE

     WHEN 3
         CASE f_subcuenta
--ahorro retiro
            WHEN 1
                 LET saldof3re = saldof3re + f_monto_pes
            WHEN 2
                 LET saldof3cv = saldof3cv + f_monto_pes
            WHEN 6
                 LET saldof3cv = saldof3cv + f_monto_pes
            WHEN 9
                 LET saldof3cv = saldof3cv + f_monto_pes
            WHEN 5
                 LET saldof3cs = saldof3cs + f_monto_pes
            WHEN 7
                 LET saldof3ra = saldof3ra + f_monto_pes
            WHEN 13
                 LET saldof3si = saldof3si + f_monto_pes
--ahorro voluntario
            WHEN 3
                 LET saldof3vo = saldof3vo + f_monto_pes
            WHEN 10
                 LET saldof3vo = saldof3vo + f_monto_pes
            WHEN 11
                 LET saldof3cr = saldof3cr + f_monto_pes
            WHEN 12
                 LET saldof3cr = saldof3cr + f_monto_pes
            WHEN 15
                 LET saldof3ah = saldof3ah + f_monto_pes
            WHEN 16
                 LET saldof3ah = saldof3ah + f_monto_pes
--ahorro vivienda
         END CASE

     WHEN 11
         CASE f_subcuenta
            WHEN 4
                 LET saldofvi = saldofvi + f_monto_pes
            WHEN 8
                 LET saldofva = saldofva + f_monto_pes
            WHEN 14
                 LET saldofvs = saldofvs + f_monto_pes
         END CASE
    END CASE

      LET f_monto_pes = 0
      LET f_subcuenta = ""
      LET f_siefore   = ""
   END FOREACH

--totales por subcuenta
    LET saldofre = saldof1re + saldof2re + saldof3re
    LET saldofcv = saldof1cv + saldof2cv + saldof3cv
    LET saldofcs = saldof1cs + saldof2cs + saldof3cs
    LET saldofra = saldof1ra + saldof2ra + saldof3ra
    LET saldofsi = saldof1si + saldof2si + saldof3si

    LET saldofvo = saldof1vo + saldof2vo + saldof3vo
    LET saldofcr = saldof1cr + saldof2cr + saldof3cr
    LET saldofah = saldof1ah + saldof2ah + saldof3ah

--totales por siefore
    LET tot_sie01_rcv = saldof1re + saldof1cv + saldof1cs +
                        saldof1ra + saldof1si
    LET tot_sie01_vol = saldof1vo + saldof1cr + saldof1ah

    LET tot_sie02_rcv = saldof2re + saldof2cv + saldof2cs +
                        saldof2ra + saldof2si
    LET tot_sie02_vol = saldof2vo + saldof2cr + saldof2ah

    LET tot_sie03_rcv = saldof3re + saldof3cv + saldof3cs +
                        saldof3ra + saldof3si
    LET tot_sie03_vol = saldof3vo + saldof3cr + saldof3ah

--importe1 ahorro retiro
   LET importe1 =  saldofre + saldofcv + saldofcs + saldofra + saldofsi

--importe4 ahorro voluntario
   LET importe4 = saldofvo + saldofcr + saldofah

--importe2 ahorro vivienda
   LET importe2 = saldofvi + saldofva + saldofvs

--importe3 total aportes
   LET importe3 = importe1 + importe2 + importe4

END FUNCTION
############################################################
FUNCTION datos_afore()
#da-------------------
   DEFINE reg_dom  RECORD LIKE tab_afore_local.*
   DEFINE reg_dom1 RECORD LIKE tab_unidad.*

   SELECT A.*
   INTO   reg_dom.*
   FROM   tab_afore_local A

   SELECT B.*
   INTO   reg_dom1.*
   FROM   tab_unidad B

   LET razon_social  = reg_dom.razon_social CLIPPED

#osb
   DECLARE cur_sie CURSOR FOR
   SELECT D.codigo_siefore,
          D.razon_social
   FROM   tab_siefore_local D
   WHERE  D.codigo_siefore NOT IN(0,11)
   ORDER BY 1

   LET k = 1

   FOREACH cur_sie INTO reg_sie[k].*
         CASE reg_sie[k].cod_sie
            WHEN 1 LET nom_sie1 = reg_sie[k].desc_sie CLIPPED
            WHEN 2 LET nom_sie2 = reg_sie[k].desc_sie CLIPPED
            WHEN 3 LET nom_sie3 = reg_sie[k].desc_sie CLIPPED
         END CASE
      LET k = k + 1
   END FOREACH

END FUNCTION #datos_afore
############################################################
FUNCTION cza_individual()
#ci----------------------
   START REPORT CZAINDD TO CZA_IND
      OUTPUT TO REPORT CZAINDD()
   FINISH REPORT CZAINDD

   LET sed = "sed -e '/^$/d' ",CZA_IND," > ",CZAIND
   RUN sed
END FUNCTION
############################################################
FUNCTION detalle_sar()

   DEFINE
       l_desc_mov      CHAR(45),
       l_id_aportante  CHAR(40),
       l_codigo	       CHAR(10),
       l_cod1	       SMALLINT,
       vtipo_mov       SMALLINT,
       l_sufijo	       CHAR(10)

   LET salapert    = saldoira
   LET vabonos_sar = 0
   LET vcargos_sar = 0
   LET vtipo_mov   = ""
   INITIALIZE det_sar TO NULL

   DECLARE cur_sar CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          c.descripcion,
          a.tipo_movimiento
   FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta = 7
   AND    b.subct_cod = a.subcuenta
   AND    c.codigo = a.tipo_movimiento
   ORDER  BY 1

   START REPORT SAR922 TO SAR_92
      LET i_cont_de_reg = 0
      FOREACH cur_sar INTO det_sar.*,
                           vtipo_mov

         LET i_cont_de_reg     = i_cont_de_reg + 1
         LET i_total_registros = i_total_registros + 1
         LET l_codigo  = det_sar.id_aportante

         IF det_sar.id_aportante MATCHES "??-*" THEN
            LET l_sufijo = det_sar.id_aportante[4,6]
         END IF

         IF det_sar.m_pesos >= 0 THEN
            LET l_desc_mov  = "TRASPASO SAR 92"
            LET vabonos_sar = vabonos_sar + det_sar.m_pesos

            CASE
               #La comision por saldo redondeada a veces = 0
               WHEN l_codigo MATCHES "METLIFE*"
                    LET l_desc_mov     = "COMISION POR SALDO"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AF*"
                    LET l_desc_mov     = "TRASPASO SAR 92"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "AC*"
                    LET l_desc_mov     = "TRASPASO COMPLEMENTARIO"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "AA*"
                    LET l_desc_mov     = "TRASPASO SAR 92"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "AS*"
                    LET l_desc_mov     = "TRASPASO COMPLEMENTARIO"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "TI*"
                    LET l_desc_mov     = "TRASPASO SAR 92"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod   = l_sufijo
               WHEN l_codigo MATCHES "IN*"
                    LET l_desc_mov     = "TRASPASO SAR 92"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod   = l_sufijo
               WHEN l_codigo MATCHES "CI*"
                    LET l_desc_mov     = "TRASPASO COMPLEMENTARIO SAR 92"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod   = l_sufijo
               WHEN l_codigo MATCHES "IC*"
                    LET l_desc_mov     = "TRASPASO COMPLEMENTARIO SAR 92"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod   = l_sufijo
               WHEN l_codigo MATCHES "DI*"
                    LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO SAR 92"
                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_sufijo #OJO: cambiar clave
               WHEN l_codigo MATCHES "DC*"
                    LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO SAR 92"
                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_sufijo #OJO: cambiar clave
               WHEN l_codigo MATCHES "RET*"
                    IF vtipo_mov = 10 THEN
                       LET l_desc_mov     = "IMPUESTOS RETENIDOS"
                       LET l_id_aportante = "GOBIERNO FEDERAL"
                    ELSE
                       LET l_desc_mov     = "RETIRO SAR 92"
                       LET l_id_aportante = "TRABAJADOR"
                    END IF
               WHEN l_codigo MATCHES "TR*"
                    LET l_desc_mov     = "TRASPASO SAR 92"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "TC*"
                    LET l_desc_mov     = "TRASPASO SAR 92"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UN*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UC*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UM*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "UQ*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "UR-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "US-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "UT-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UV-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UW-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UX-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UY-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "UZ-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod   = l_sufijo
               WHEN l_codigo MATCHES "TDA-*"
                    LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
                    LET   l_id_aportante = "SB1"
               WHEN l_codigo MATCHES "SOLORDSEL"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
               WHEN l_codigo MATCHES "DECIMOS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
               WHEN l_codigo MATCHES "CORTE"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "CORTE TRANSVERSAL"
               WHEN l_codigo MATCHES "INDEBIDO"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA"
               WHEN l_codigo MATCHES "INDIMSS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
               WHEN l_codigo MATCHES "INDISSS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
               WHEN l_codigo MATCHES "INDUNIF"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
               WHEN l_codigo MATCHES "INDTRAP"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
               WHEN l_codigo MATCHES "SOLASIG"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"

               OTHERWISE
                  LET l_cod1 = l_codigo

                  SELECT tab_icefa.icefa_desc
                  INTO   l_id_aportante
                  FROM   tab_icefa
                  WHERE  tab_icefa.icefa_cod = l_cod1
            END CASE
         ELSE                              # Monto negativo
            LET vcargos_sar = vcargos_sar + det_sar.m_pesos
            CASE
               WHEN l_codigo MATCHES "METLIFE*"
                    LET l_desc_mov     = "COMISION POR SALDO"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AA*"
                    LET l_desc_mov     = "COMISION POR SALDO"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AS*"
                    LET l_desc_mov     = "COMISION POR SALDO"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AF*"
                    LET l_desc_mov  = "TRASPASO SAR 92"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AC*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "TI*"
                    LET l_desc_mov  = "TRASPASO SAR 92"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "IN*"
                    LET l_desc_mov  = "TRASPASO SAR 92"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "CI*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO SAR 92"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "IC*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO SAR 92"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "DI*"
                    LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO SAR 92"
                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_sufijo
                    #LET l_id_aportante = razon_social CLIPPED #confirmar arc
               WHEN l_codigo MATCHES "DC*"
                    LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO SAR 92"
                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_sufijo
                    #LET l_id_aportante = razon_social CLIPPED #confirmar arc
               WHEN l_codigo MATCHES "RET*"
                    IF vtipo_mov = 10 THEN
                       LET l_desc_mov     = "IMPUESTOS RETENIDOS"
                       LET l_id_aportante = "GOBIERNO FEDERAL"
                    ELSE
                       LET l_desc_mov     = "RETIRO SAR 92"
                       LET l_id_aportante = "TRABAJADOR"
                    END IF
               WHEN l_codigo MATCHES "TR*"
                    LET l_desc_mov     = "TRASPASO SAR 92"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "TC*"
                    LET l_desc_mov     = "TRASPASO SAR 92"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UN*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UC*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UM*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UQ*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UR-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "US-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UT-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UV-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UW-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UX-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UY-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UZ-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "TDC-*"
                    LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
                    LET   l_id_aportante = "SB2"
               WHEN l_codigo MATCHES "SOLORDSEL"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
               WHEN l_codigo MATCHES "DECIMOS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
               WHEN l_codigo MATCHES "CORTE"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "CORTE TRANSVERSAL"
               WHEN l_codigo MATCHES "INDEBIDO"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA"
               WHEN l_codigo MATCHES "INDIMSS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
               WHEN l_codigo MATCHES "INDISSS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
               WHEN l_codigo MATCHES "INDUNIF"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
               WHEN l_codigo MATCHES "INDTRAP"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
               WHEN l_codigo MATCHES "SOLASIG"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
            END CASE
         END IF

      LET det_sar.id_aportante = l_id_aportante
      --LET det_sar.desc_mov     = l_desc_mov
      LET det_sar.desc_mov     = det_sar.tipo_char
      --LET det_sar.tipo_char    = ""

         OUTPUT TO REPORT SAR922(det_sar.*)
         LET l_desc_mov     = ""
         LET l_id_aportante = ""

         LET i = i + 1
      END FOREACH
   FINISH REPORT SAR922

   LET x_abono_sar = vabonos_sar
   LET x_cargo_sar = vcargos_sar

   LET sed = "sed -e '/^$/d' ",SAR_92," > ",SAR92
   RUN sed

   START REPORT SUMSAR_92 TO SUM_SAR_92
       OUTPUT TO REPORT SUMSAR_92()
   FINISH REPORT SUMSAR_92

   LET sed = "sed -e '/^$/d' ",SUM_SAR_92," > ",SUMSAR92
   RUN sed
END FUNCTION #detalle_sar
############################################################
FUNCTION detalle_sar_issste()

   DEFINE #loc #char
       l_desc_mov	     CHAR(45),
       l_id_aportante  CHAR(40),
       l_codigo	       CHAR(10),
       l_cod1	         SMALLINT,
       vtipo_mov       SMALLINT,
       l_sufijo	       CHAR(10)

   LET salapert    = saldoira
   LET vabonos_sar = 0
   LET vcargos_sar = 0
   LET vtipo_mov   = ""
   INITIALIZE det_saris TO NULL

   DECLARE cur_sar_issste CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          c.descripcion,
          a.folio,
          a.folio_sua,
          a.tipo_movimiento
   FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta = 13
   AND     b.subct_cod        = a.subcuenta
   AND     c.codigo = a.tipo_movimiento
   ORDER  BY 1

   START REPORT SAR92ISSSTE TO SAR_ISSSTE
      LET i_cont_de_reg = 0
      FOREACH cur_sar_issste INTO det_saris.*,
                                  vtipo_mov
         LET i_cont_de_reg     = i_cont_de_reg + 1
         LET i_total_registros = i_total_registros + 1
         LET l_codigo  = det_sar.id_aportante

         IF det_sar.id_aportante MATCHES "??-*" THEN
            LET l_sufijo = det_saris.id_aportante[4,6]
         END IF

         IF det_sar.m_pesos >= 0 THEN
            LET l_desc_mov  = "TRASPASO SAR ISSSTE"
            LET vabonos_sar = vabonos_sar + det_saris.m_pesos

            CASE
               #La comision por saldo redondeada a veces = 0
               WHEN l_codigo MATCHES "METLIFE*"
                    LET l_desc_mov = "COMISION POR SALDO"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AF*"
                    LET l_desc_mov  = "TRASPASO SAR ISSSTE"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AC*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AA*"
                    LET l_desc_mov  = "TRASPASO SAR ISSSTE"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AS*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "TI*"
                    LET l_desc_mov  = "TRASPASO SAR ISSSTE"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "IN*"
                    LET l_desc_mov  = "TRASPASO SAR ISSSTE"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "CI*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO SAR ISSSTE"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "IC*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO SAR ISSSTE"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "DI*"
                    LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO SAR ISSSTE"
                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_sufijo #OJO: cambiar clave
               WHEN l_codigo MATCHES "DC*"
                    LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO SAR ISSSTE"
                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_sufijo #OJO: cambiar clave
               WHEN l_codigo MATCHES "RET*"
                    IF vtipo_mov = 10 THEN
                       LET l_desc_mov     = "IMPUESTOS RETENIDOS"
                       LET l_id_aportante = "GOBIERNO FEDERAL"
                    ELSE
                       LET l_desc_mov     = "RETIRO SAR ISSSTE"
                       LET l_id_aportante = "TRABAJADOR"
                    END IF
               WHEN l_codigo MATCHES "TR*"
                    LET l_desc_mov     = "TRASPASO SAR ISSSTE"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "TC*"
                    LET l_desc_mov     = "TRASPASO SAR ISSSTE"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UN*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UC*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UM*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UQ*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UR-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "US-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UT-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UV-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UW-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UX-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UY-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UZ-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "TDA-*"
                    LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
                    LET   l_id_aportante = "SB1"
               WHEN l_codigo MATCHES "SOLORDSEL"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
               WHEN l_codigo MATCHES "DECIMOS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
               WHEN l_codigo MATCHES "CORTE"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "CORTE TRANSVERSAL"
               WHEN l_codigo MATCHES "INDEBIDO"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA"
               WHEN l_codigo MATCHES "INDIMSS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
               WHEN l_codigo MATCHES "INDISSS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
               WHEN l_codigo MATCHES "INDUNIF"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
               WHEN l_codigo MATCHES "INDTRAP"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
               WHEN l_codigo MATCHES "SOLASIG"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
               OTHERWISE
                    LET l_cod1 = l_codigo

                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_cod1
            END CASE
         ELSE                              # Monto negativo
            LET vcargos_sar = vcargos_sar + det_sar.m_pesos

            CASE
               WHEN l_codigo MATCHES "METLIFE"
                    LET l_desc_mov     = "COMISION POR SALDO"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AA*"
                    LET l_desc_mov     = "COMISION POR SALDO"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AS*"
                    LET l_desc_mov     = "COMISION POR SALDO"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AF*"
                    LET l_desc_mov  = "TRASPASO SAR ISSSTE"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AC*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "TI*"
                    LET l_desc_mov  = "TRASPASO SAR ISSSTE"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "IN*"
                    LET l_desc_mov  = "TRASPASO SAR ISSSTE"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "CI*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO SAR ISSSTE"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "IC*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO SAR ISSSTE"
                    SELECT icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  icefa_cod = l_sufijo
               WHEN l_codigo MATCHES "DI*"
                   LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO SAR ISSSTE"
                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_sufijo
                    #LET l_id_aportante = razon_social CLIPPED #confirmar arc
               WHEN l_codigo MATCHES "DC*"
                   LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO SAR ISSSTE"
                    SELECT tab_icefa.icefa_desc
                    INTO   l_id_aportante
                    FROM   tab_icefa
                    WHERE  tab_icefa.icefa_cod = l_sufijo
                    #LET l_id_aportante = razon_social CLIPPED #confirmar arc
               WHEN l_codigo MATCHES "RET*"
                    IF vtipo_mov = 10 THEN
                       LET l_desc_mov     = "IMPUESTOS RETENIDOS"
                       LET l_id_aportante = "GOBIERNO FEDERAL"
                    ELSE
                       LET l_desc_mov     = "RETIRO SAR ISSSTE"
                       LET l_id_aportante = "TRABAJADOR"
                    END IF
               WHEN l_codigo MATCHES "TR*"
                    LET l_desc_mov     = "TRASPASO SAR ISSSTE"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "TC*"
                    LET l_desc_mov     = "TRASPASO SAR ISSSTE"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UN*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UC*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UM*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UQ*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UR-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "US-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UT-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UV-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UW-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UX-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UY-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UZ-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "TDC-*"
                    LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
                    LET   l_id_aportante = "SB2"
               WHEN l_codigo MATCHES "SOLORDSEL"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
               WHEN l_codigo MATCHES "DECIMOS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
               WHEN l_codigo MATCHES "CORTE"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "CORTE TRANSVERSAL"
               WHEN l_codigo MATCHES "INDEBIDO"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA"
               WHEN l_codigo MATCHES "INDIMSS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
               WHEN l_codigo MATCHES "INDISSS"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
               WHEN l_codigo MATCHES "INDUNIF"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
               WHEN l_codigo MATCHES "INDTRAP"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
               WHEN l_codigo MATCHES "SOLASIG"
                    LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                    LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
            END CASE
         END IF

      LET det_saris.id_aportante = l_id_aportante
      --LET det_saris.desc_mov     = l_desc_mov
      LET det_saris.desc_mov     = det_saris.tipo_char
      --LET det_saris.tipo_char    = ""

         OUTPUT TO REPORT SAR92ISSSTE(det_saris.*)
         LET l_desc_mov     = ""
         LET l_id_aportante = ""

         LET i = i + 1
      END FOREACH
   FINISH REPORT SAR92ISSSTE

   LET x_abono_saris = vabonos_sar
   LET x_cargo_saris = vcargos_sar

   LET sed = "sed -e '/^$/d' ",SAR_ISSSTE," > ",SARISSSTE
   RUN sed

   START REPORT SUMSAR_92_ISSSTE TO SUM_SAR_ISSSTE
       OUTPUT TO REPORT SUMSAR_92_ISSSTE()
   FINISH REPORT SUMSAR_92_ISSSTE

   LET sed = "sed -e '/^$/d' ",SUM_SAR_ISSSTE," > ",SUMSARISSSTE
   RUN sed
END FUNCTION #detalle_sar_issste
############################################################
FUNCTION detalle_rcv()

   DEFINE vcomision           SMALLINT,
          xcomision           DECIMAL(16,6),
          vacciones           DECIMAL(16,6),
          l_desc_mov          CHAR(45),
          l_tmovimiento       CHAR(30),
          l_tipo_char         CHAR(45),
          l_id_aportante      CHAR(40),
          l_codigo            CHAR(10),
          l_sufijo            CHAR(10),
          l_subct_desc        CHAR(40),
          l_char              CHAR(45),
	  l_id                CHAR(40)

   CREATE TEMP TABLE rcv_movimiento
     (fecha                 DATE,
      desc_mov              CHAR(45),
      id_aportante          CHAR(30),
      m_pesos               DECIMAL(10,2),
      subcuenta             SMALLINT,
      siefore_cod           SMALLINT,
      tipo_movimiento       SMALLINT,
      tipo_char             CHAR(29),
      folio                 INTEGER)

   LET vcargos_rcv = 0
   LET vabonos_rcv = 0
   INITIALIZE det_rcv TO NULL
#--------------------------------------Traspasos Recibidos
   DECLARE cur_rcv_tra CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          a.tipo_movimiento,
          c.descripcion,
          a.folio,
          a.folio_sua,
          a.consecutivo_lote
   FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta        = 1
   AND     b.subct_cod     = a.subcuenta
   AND     c.codigo = a.tipo_movimiento
   ORDER  BY 1,3

   LET i = 1

   LET i_cont_de_reg = 0
   FOREACH cur_rcv_tra INTO det_rcv.*,
                            vfolio_sua,
                            vconsecutivo

      LET l_codigo     = det_rcv.id_aportante

      IF det_rcv.id_aportante MATCHES "??-*" THEN
         LET l_sufijo = det_rcv.id_aportante[4,6]
      END IF

      IF det_rcv.m_pesos < 0 THEN
         LET vcargos_rcv = vcargos_rcv + det_rcv.m_pesos
      ELSE
         LET vabonos_rcv = vabonos_rcv + det_rcv.m_pesos
      END IF

      CASE
         WHEN l_codigo MATCHES "UR-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "US-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "UY-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "UZ-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "MQ*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "MC*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "MA*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "MS*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "AF*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "AC*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "AA*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
              IF det_rcv.m_pesos < 0 THEN
                 LET l_id_aportante = razon_social CLIPPED
              END IF
         WHEN l_codigo MATCHES "AS*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
              IF det_rcv.m_pesos < 0 THEN
                 LET l_id_aportante = razon_social CLIPPED
              END IF
         WHEN l_codigo MATCHES "METLIFE"
              LET l_desc_mov     = "COMISION POR SALDO"
              LET l_id_aportante = razon_social CLIPPED
         WHEN l_codigo MATCHES "BANXICO"
              LET l_desc_mov    = "INTERESES APORTACIONES COMP. RETIRO"
              LET l_id_aportante = "BANXICO"
         WHEN l_codigo MATCHES "AL-*"
              LET l_tipo_char = "CORRECCION "
              LET l_id_aportante = razon_social CLIPPED
         WHEN l_codigo MATCHES "TDA-*"
              LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
              LET   l_id_aportante = "SB1"
         WHEN l_codigo MATCHES "SOLORDSEL"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
         WHEN l_codigo MATCHES "DECIMOS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
         WHEN l_codigo MATCHES "CORTE"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "CORTE TRANSVERSAL"
         WHEN l_codigo MATCHES "INDEBIDO"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA"
         WHEN l_codigo MATCHES "INDIMSS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
         WHEN l_codigo MATCHES "INDISSS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
         WHEN l_codigo MATCHES "INDUNIF"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
         WHEN l_codigo MATCHES "INDTRAP"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
         WHEN l_codigo MATCHES "SOLASIG"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
      END CASE

      LET det_rcv.id_aportante = l_id_aportante
      --LET det_rcv.desc_mov     = l_tipo_char
      LET det_rcv.desc_mov     = det_rcv.tipo_char
      --LET det_rcv.tipo_char    = ""

      INSERT INTO rcv_movimiento
      VALUES(det_rcv.*) #RCV

      LET l_id_aportante = ""
      LET l_tipo_char    = ""
   END FOREACH

   LET x_cargo_rcv = vcargos_rcv
   LET x_abono_rcv = vabonos_rcv

#--------------------------ordena por fecha los movimientos de RCV
   DECLARE rcv_mov CURSOR FOR
   SELECT *
   FROM   rcv_movimiento
   ORDER BY 1

   LET i = 1
   LET i_cont_de_reg = 0

   START REPORT RRCV TO RCV
   FOREACH rcv_mov INTO det_rcv.*
      LET i_cont_de_reg = i_cont_de_reg + 1
      LET i = i + 1
      OUTPUT TO REPORT RRCV(det_rcv.*) #RCV
   END FOREACH
   FINISH REPORT RRCV

   DROP TABLE rcv_movimiento

   LET sed = "sed -e '/^$/d' ",RCV," > ",RCVV
   RUN sed

   START REPORT SSUM_RCV TO SUM_RCV
      OUTPUT TO REPORT SSUM_RCV()
   FINISH REPORT SSUM_RCV

   LET sed =  "sed -e '/^$/d' ",SUM_RCV," > ",SUMRCV
   RUN sed
END FUNCTION #detalle_rcv
##############################################################
FUNCTION detalle_cv()

   DEFINE vcomision           SMALLINT,
          xcomision           DECIMAL(16,6),
          vacciones           DECIMAL(16,6),
          l_desc_mov          CHAR(45),
          l_tmovimiento       CHAR(30),
          l_tipo_char         CHAR(45),
          l_id_aportante      CHAR(40),
          l_codigo            CHAR(10),
          l_sufijo            CHAR(10),
          l_subct_desc        CHAR(40),
          l_char              CHAR(45),
	  l_id                CHAR(40)

   CREATE TEMP TABLE cv_movimiento
       (fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER)

   LET vcargos_rcv = 0
   LET vabonos_rcv = 0
   INITIALIZE det_cv TO NULL

#--------------------------------------Traspasos Recibidos
   DECLARE cur_cv_tra CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          a.tipo_movimiento,
          c.descripcion,
          a.folio,
          a.folio_sua
   FROM   tmp_cuenta a,tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta IN (2,6,9)
   AND     b.subct_cod  = a.subcuenta
   AND     c.codigo     = a.tipo_movimiento
   ORDER  BY 1,3

   LET i = 1

   LET i_cont_de_reg = 0
   FOREACH cur_cv_tra INTO det_cv.*,
                            vfolio_sua

      LET l_codigo = det_cv.id_aportante

      IF det_cv.id_aportante MATCHES "??-*" THEN
         LET l_sufijo = det_cv.id_aportante[4,6]
      END IF

      IF det_cv.m_pesos < 0 THEN
         LET vcargos_rcv = vcargos_rcv + det_cv.m_pesos
      ELSE
         LET vabonos_rcv = vabonos_rcv + det_cv.m_pesos
      END IF

      CASE
         WHEN l_codigo MATCHES "UR-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "US-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "UY-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "UZ-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "MQ*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "MC*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "MA*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "MS*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "AF*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "AC*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "AA*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
              IF det_cv.m_pesos < 0 THEN
                 LET l_id_aportante = razon_social CLIPPED
              END IF
         WHEN l_codigo MATCHES "AS*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
              IF det_cv.m_pesos < 0 THEN
                 LET l_id_aportante = razon_social CLIPPED
              END IF
         WHEN l_codigo MATCHES "AL-*"
              LET l_tipo_char = "CORRECCION "
              LET l_id_aportante = razon_social CLIPPED
         WHEN l_codigo MATCHES "TDA-*"
              LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
              LET   l_id_aportante = "SB1"
         WHEN l_codigo MATCHES "SOLORDSEL"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
         WHEN l_codigo MATCHES "DECIMOS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
         WHEN l_codigo MATCHES "CORTE"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "CORTE TRANSVERSAL"
         WHEN l_codigo MATCHES "INDEBIDO"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA"
         WHEN l_codigo MATCHES "INDIMSS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
         WHEN l_codigo MATCHES "INDISSS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
         WHEN l_codigo MATCHES "INDUNIF"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
         WHEN l_codigo MATCHES "INDTRAP"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
         WHEN l_codigo MATCHES "SOLASIG"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
      END CASE

      LET det_rcv.id_aportante = l_id_aportante
      --LET det_rcv.desc_mov     = l_tipo_char
      LET det_rcv.desc_mov     = det_rcv.tipo_char
      --LET det_rcv.tipo_char    = ""

      INSERT INTO cv_movimiento
      VALUES(det_cv.*) #CV

      LET l_id_aportante = ""
      LET l_tipo_char    = ""
   END FOREACH

   LET x_cargo_cv = vcargos_rcv
   LET x_abono_cv = vabonos_rcv

   DECLARE cv_mov CURSOR FOR
   SELECT *
   FROM   cv_movimiento
   ORDER BY 1

   LET i = 1
   LET i_cont_de_reg = 0

   START REPORT RECV TO CV
   FOREACH cv_mov INTO det_cv.*
      LET i_cont_de_reg = i_cont_de_reg + 1
      LET i = i + 1
      OUTPUT TO REPORT RECV(det_cv.*) #CV
   END FOREACH
   FINISH REPORT RECV

   DROP TABLE cv_movimiento

   LET sed = "sed -e '/^$/d' ",CV," > ",CVV
   RUN sed

   START REPORT SSUM_CV TO SUM_CV
      OUTPUT TO REPORT SSUM_CV()
   FINISH REPORT SSUM_CV

   LET sed =  "sed -e '/^$/d' ",SUM_CV," > ",SUMCV
   RUN sed

END FUNCTION #detalle_cv
############################################################
FUNCTION detalle_cs()

   DEFINE vcomision           SMALLINT,
          xcomision           DECIMAL(16,6),
          vacciones           DECIMAL(16,6),
          l_desc_mov          CHAR(45),
          l_tmovimiento       CHAR(30),
          l_tipo_char         CHAR(45),
          l_id_aportante      CHAR(40),
          l_codigo            CHAR(10),
          l_sufijo            CHAR(10),
          l_subct_desc        CHAR(40),
          l_char              CHAR(45),
	  l_id                CHAR(40)

   CREATE TEMP TABLE cs_movimiento
    (fecha                 DATE,
     desc_mov              CHAR(45),
     id_aportante          CHAR(30),
     m_pesos               DECIMAL(10,2),
     subcuenta             SMALLINT,
     siefore_cod           SMALLINT,
     tipo_movimiento       SMALLINT,
     tipo_char             CHAR(29),
     folio                 INTEGER)

   LET vcargos_rcv = 0
   LET vabonos_rcv = 0
   INITIALIZE det_cs TO NULL

#--------------------------------------Traspasos Recibidos
   DECLARE cur_cs_tra CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          a.tipo_movimiento,
          c.descripcion,
          a.folio,
          a.folio_sua
   FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta  = 5
   AND     b.subct_cod        = a.subcuenta
   AND     c.codigo = a.tipo_movimiento
   ORDER  BY 1,3

   LET i = 1

   LET i_cont_de_reg = 0
   FOREACH cur_cs_tra INTO det_cs.*,
                           vfolio_sua

      LET l_codigo     = det_cs.id_aportante

      IF det_cs.id_aportante MATCHES "??-*" THEN
         LET l_sufijo = det_cs.id_aportante[4,6]
      END IF

      IF det_cs.m_pesos < 0 THEN
         LET vcargos_rcv = vcargos_rcv + det_cs.m_pesos
      ELSE
         LET vabonos_rcv = vabonos_rcv + det_cs.m_pesos
      END IF

      CASE
         WHEN l_codigo MATCHES "UR-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "US-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "UY-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "UZ-*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "UNIFICACION DE CUENTAS"
         WHEN l_codigo MATCHES "MQ*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "MC*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "MA*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "MS*"
              SELECT desc_afore
              INTO   l_id_aportante
              FROM   tab_prestadora
              WHERE  cod_afore = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "AF*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
         WHEN l_codigo MATCHES "AC*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
         WHEN l_codigo MATCHES "AA*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO RCV"
              IF det_cs.m_pesos < 0 THEN
                 LET l_id_aportante = razon_social CLIPPED
              END IF
         WHEN l_codigo MATCHES "AS*"
              SELECT afore_desc
              INTO   l_id_aportante
              FROM   tab_afore
              WHERE  afore_cod = l_sufijo
              LET l_tipo_char  = "TRASPASO COMPLEMENTARIO"
              IF det_cs.m_pesos < 0 THEN
                 LET l_id_aportante = razon_social CLIPPED
              END IF
         WHEN l_codigo MATCHES "AL-*"
              LET l_tipo_char = "CORRECCION "
              LET l_id_aportante = razon_social CLIPPED
         WHEN l_codigo MATCHES "TDA-*"
              LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
              LET   l_id_aportante = "SB1"
         WHEN l_codigo MATCHES "SOLORDSEL"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
         WHEN l_codigo MATCHES "DECIMOS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
         WHEN l_codigo MATCHES "CORTE"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "CORTE TRANSVERSAL"
         WHEN l_codigo MATCHES "INDEBIDO"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA"
         WHEN l_codigo MATCHES "INDIMSS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
         WHEN l_codigo MATCHES "INDISSS"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
         WHEN l_codigo MATCHES "INDUNIF"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
         WHEN l_codigo MATCHES "INDTRAP"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
         WHEN l_codigo MATCHES "SOLASIG"
              LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
              LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
      END CASE

      LET det_rcv.id_aportante = l_id_aportante
      --LET det_rcv.desc_mov     = l_tipo_char
      LET det_rcv.desc_mov     = det_rcv.tipo_char
      --LET det_rcv.tipo_char    = ""

      INSERT INTO cs_movimiento
      VALUES(det_cs.*) #CS

      LET l_id_aportante = ""
      LET l_tipo_char    = ""
   END FOREACH

   LET x_cargo_cs = vcargos_rcv
   LET x_abono_cs = vabonos_rcv

   DECLARE cs_mov CURSOR FOR
   SELECT *
   FROM   cs_movimiento
   ORDER BY 1

   LET i = 1
   LET i_cont_de_reg = 0

   START REPORT RECS TO CS
   FOREACH cs_mov INTO det_cs.*
      LET i_cont_de_reg = i_cont_de_reg + 1
      LET i = i + 1
      OUTPUT TO REPORT RECS(det_cs.*) #CS
   END FOREACH
   FINISH REPORT RECS

   DROP TABLE cs_movimiento

   LET sed = "sed -e '/^$/d' ",CS," > ",CSV
   RUN sed

   START REPORT SSUM_CS TO SUM_CS
      OUTPUT TO REPORT SSUM_CS()
   FINISH REPORT SSUM_CS

   LET sed =  "sed -e '/^$/d' ",SUM_CS," > ",SUMCS
   RUN sed
END FUNCTION #detalle_cs
############################################################
FUNCTION detalle_viv()

   DEFINE l_desc_mov       CHAR(45),
          l_tipo_char	   CHAR(45),
          l_id_aportante   CHAR(40),
          l_codigo	   CHAR(10),
          l_cod1           SMALLINT,
          l_sufijo	   CHAR(10),
          l_subct_desc	   CHAR(40)

#---------------------------------------------- VIV 92
   LET vcargos_viv92 = 0
   LET vabonos_viv92 = 0
   INITIALIZE det_viv92 TO NULL

   DECLARE cur_viv92 CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          a.tipo_movimiento,
          c.descripcion,
          a.folio,
          a.fecha_valor
   FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta   = 8
   AND    b.subct_cod = a.subcuenta
   AND    c.codigo    = a.tipo_movimiento
   ORDER  BY 1,10

   LET i = 1
   START REPORT VVIV_92 TO VIV_92

   LET i_cont_de_reg = 0
   FOREACH cur_viv92 INTO det_viv92.*,
                          vfecha_valor92
      LET i_cont_de_reg     = i_cont_de_reg + 1
      LET i_total_registros = i_total_registros + 1
      LET l_codigo  = det_viv92.id_aportante

      IF det_viv92.id_aportante MATCHES "??-*" THEN
         LET l_sufijo = det_viv92.id_aportante[4,6]
      END IF

      IF det_viv92.m_pesos >= 0 THEN
         LET vabonos_viv92 = vabonos_viv92 + det_viv92.m_pesos
         LET vperiodo_int = periodo_int(vfecha_valor92)

         CASE
            WHEN l_codigo MATCHES "ACR*"
                 LET l_desc_mov     = "CREDITO DE VIVIENDA"
                 LET l_id_aportante = "INFONAVIT"
            WHEN l_codigo MATCHES "ACR. U*"
                 LET l_desc_mov     = "CREDITO DE VIVIENDA"
                 LET l_id_aportante = "INFONAVIT"
            WHEN l_codigo = "DEV-INF"
                 LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                 LET l_id_aportante = "INFONAVIT"
            WHEN l_codigo MATCHES "DEV. I*"
                 LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                 LET l_id_aportante = "INFONAVIT"
            WHEN l_codigo MATCHES "MQ*"
                 LET l_desc_mov  = "TRASPASO VIVIENDA 92"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MC*"
                 LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 92"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "AF*"
                 LET l_desc_mov  = "TRASPASO VIVIENDA 92"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AC*"
                 LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 92"
                 SELECT tab_icefa.icefa_desc
                 INTO   l_id_aportante
                 FROM   tab_icefa
                 WHERE  tab_icefa.icefa_cod = l_sufijo #OJO: cambiar clave
            WHEN l_codigo MATCHES "AA*"
                 LET l_desc_mov  = "TRASPASO VIVIENDA 92"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AS*"
                 LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 92"
                 SELECT tab_icefa.icefa_desc
                 INTO   l_id_aportante
                 FROM   tab_icefa
                 WHERE  tab_icefa.icefa_cod = l_sufijo #OJO: cambiar clave
            WHEN l_codigo MATCHES "INF*"
                 SELECT tasa_valor
                 INTO   vtasa_oper
                 FROM   tab_tasa_ordinaria
                 WHERE  tasa_origen = "VIV"
                 AND    tasa_fecha  = vfecha_valor92

                 LET l_desc_mov = "INTERESES ",vtasa_oper USING "###.####","% ",
                                   vperiodo_int
                 LET l_id_aportante = "INFONAVIT"
            WHEN l_codigo MATCHES "REM*"
                 SELECT tasa_valor
                 INTO   vtasa_oper
                 FROM   tab_tasa_remanente
                 WHERE  tasa_origen = "VIV"
                 AND    tasa_fecha  = vfecha_valor92

                 LET l_desc_mov = "INTERESES PAGO TRECE ",vtasa_oper USING "###.####","% ",
                                  vperiodo_int
                 LET l_id_aportante = "INFONAVIT"
            WHEN l_codigo MATCHES "DI*"
                 LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO VIV 92"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "DC*"
                 LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO VIV 92"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "RET*"
                 LET l_desc_mov     = "RETIRO VIVIENDA 92"
                 LET l_id_aportante = "TRABAJADOR"
            WHEN l_codigo MATCHES "UN*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UC*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UM*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UQ*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UR-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "US-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UT-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UV-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UW-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UX-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UY-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UZ-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AL-*"
                 LET l_desc_mov = "CORRECCION "
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "SALDO"
                 LET l_desc_mov = "PARTICIPACIONES VIVIENDA"
                 LET l_id_aportante = "INFONAVIT"
            OTHERWISE
                 LET l_desc_mov  = "TRASPASO VIVIENDA 92"
                 SELECT tab_icefa.icefa_desc
                 INTO   l_id_aportante
                 FROM   tab_icefa
                 WHERE  tab_icefa.icefa_cod = l_sufijo
          END CASE
       ELSE                                    # Monto negativo
          LET vcargos_viv92 = vcargos_viv92 + det_viv92.m_pesos

          CASE
             WHEN l_codigo MATCHES "ACR*"
                  LET l_desc_mov     = "CREDITO DE VIVIENDA"
                  LET l_id_aportante = "INFONAVIT"
             WHEN l_codigo MATCHES "ACR-U*"
                  LET l_desc_mov     = "CREDITO DE VIVIENDA"
                  LET l_id_aportante = "INFONAVIT"
             WHEN l_codigo = "DEV-INF"
                  LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                  LET l_id_aportante = "INFONAVIT"
             WHEN l_codigo MATCHES "DEV. I*"
                  LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                  LET l_id_aportante = "INFONAVIT"
             WHEN l_codigo MATCHES "TR*"
                  LET l_desc_mov  = "TRASPASO VIVIENDA 92"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "TC*"
                  LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 92"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "AA*"
                  LET l_desc_mov  = "TRASPASO VIVIENDA 92"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "AS*"
                  LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 92"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "RET*"
                  LET l_desc_mov     = "RETIRO VIVIENDA 92"
                  LET l_id_aportante = "TRABAJADOR"
             WHEN l_codigo MATCHES "UN*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "UC*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "UM*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  SELECT afore_desc
                  INTO   l_id_aportante
                  FROM   tab_afore
                  WHERE  afore_cod = l_sufijo
             WHEN l_codigo MATCHES "UQ*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  SELECT afore_desc
                  INTO   l_id_aportante
                  FROM   tab_afore
                  WHERE  afore_cod = l_sufijo
             WHEN l_codigo MATCHES "UR-*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  SELECT afore_desc
                  INTO   l_id_aportante
                  FROM   tab_afore
                  WHERE  afore_cod = l_sufijo
             WHEN l_codigo MATCHES "US-*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  SELECT afore_desc
                  INTO   l_id_aportante
                  FROM   tab_afore
                  WHERE  afore_cod = l_sufijo
             WHEN l_codigo MATCHES "UT-*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "UV-*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "UW-*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "UX-*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "UY-*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  SELECT afore_desc
                  INTO   l_id_aportante
                  FROM   tab_afore
                  WHERE  afore_cod = l_sufijo
             WHEN l_codigo MATCHES "UZ-*"
                  LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                  SELECT afore_desc
                  INTO   l_id_aportante
                  FROM   tab_afore
                  WHERE  afore_cod = l_sufijo
             WHEN l_codigo MATCHES "DI*"
                  LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO VIV 92"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "DC*"
                  LET l_desc_mov = "DEVOLUCION DE TRASPASO INDEBIDO VIV 92"
                  LET l_id_aportante = razon_social CLIPPED
             WHEN l_codigo MATCHES "AL-*"
                  LET l_desc_mov = "CORRECCION "
                  LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "AJUSTE"
                 LET l_desc_mov = "PARTICIPACIONES VIVIENDA"
                 LET l_id_aportante = "INFONAVIT"
          END CASE
       END IF

      LET det_viv92.id_aportante = l_id_aportante
      --LET det_viv92.desc_mov     = l_desc_mov
      LET det_viv92.desc_mov     = det_viv92.tipo_char
      --LET det_viv92.tipo_char    = ""


       OUTPUT TO REPORT VVIV_92(det_viv92.*)
       LET l_desc_mov     = ""
       LET l_id_aportante = ""
    END FOREACH
    FINISH REPORT VVIV_92

   LET x_cargo_viv92 = vcargos_viv92
   LET x_abono_viv92 = vabonos_viv92

    LET sed = "sed -e '/^$/d' ",VIV_92," > ",VIV92
    RUN sed

    START REPORT SUMVIV_92 TO SUM_VIV_92
        OUTPUT TO REPORT SUMVIV_92()
    FINISH REPORT SUMVIV_92

    LET sed = "sed -e '/^$/d' ",SUM_VIV_92," > ",SUMVIV92
    RUN sed

#------------------------------------------------- VIV 97
    LET vbimestre = " "
    LET vcargos_viv97 = 0
    LET vabonos_viv97 = 0
    INITIALIZE det_viv97 TO NULL

    DECLARE cur_viv97 CURSOR FOR
    SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          a.tipo_movimiento,
          c.descripcion,
          a.folio,
          a.folio_sua,
          a.consecutivo_lote,
          a.fecha_valor
    FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
    WHERE  subcuenta   = 4
    AND    b.subct_cod = a.subcuenta
    AND    c.codigo    = a.tipo_movimiento
    ORDER  BY 1,12

    LET i = 1
    START REPORT VVIV_97 TO VIV_97

    LET i_cont_de_reg = 0
    FOREACH cur_viv97 INTO det_viv97.*,
                           vfolio_sua,
                           vconsecutivo,
                           vfecha_valor

        LET l_codigo  = det_viv97.id_aportante

        IF det_viv97.id_aportante MATCHES "??-*" THEN
            LET l_sufijo = det_viv97.id_aportante[4,6]
        END IF

        IF det_viv97.m_pesos >= 0 THEN
            LET vabonos_viv97 = vabonos_viv97 + det_viv97.m_pesos
            CALL periodo_int(vfecha_valor)
            RETURNING vperiodo_int

            CASE
               WHEN l_codigo MATCHES "ACR*"
                    LET l_desc_mov     = "CREDITO DE VIVIENDA"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo = "ACR-ULT"
                    LET l_desc_mov     = "CREDITO DE VIVIENDA"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo = "DEV-INF"
                    LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "DEV. I*"
                    LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "PAG-EXC*"
                    LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo = "PAG-EXC-VOL"
                    LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "MQ*"
                    LET l_desc_mov  = "TRASPASO VIVIENDA"
                    SELECT desc_afore
                    INTO   l_id_aportante
                    FROM   tab_prestadora
                    WHERE  cod_afore = l_sufijo
               WHEN l_codigo MATCHES "MC*"
                    LET l_desc_mov  = "TRASPASO VIVIENDA"
                    SELECT desc_afore
                    INTO   l_id_aportante
                    FROM   tab_prestadora
                    WHERE  cod_afore = l_sufijo
               WHEN l_codigo MATCHES "MA*"
                    LET l_desc_mov  = "TRASPASO VIVIENDA"
                    SELECT desc_afore
                    INTO   l_id_aportante
                    FROM   tab_prestadora
                    WHERE  cod_afore = l_sufijo
               WHEN l_codigo MATCHES "MS*"
                    LET l_desc_mov  = "TRASPASO VIVIENDA"
                    SELECT desc_afore
                    INTO   l_id_aportante
                    FROM   tab_prestadora
                    WHERE  cod_afore = l_sufijo
               WHEN l_codigo MATCHES "AF*"
                    LET l_desc_mov  = "TRASPASO VIVIENDA"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AC*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 97"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AA*"
                    LET l_desc_mov  = "TRASPASO VIVIENDA"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AS*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 97"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "IN*"
                    SELECT tasa_valor
                    INTO   vtasa_oper
                    FROM   tab_tasa_ordinaria
                    WHERE  tasa_origen = "VIV"
                    AND    tasa_fecha  = vfecha_valor

                   LET l_desc_mov = "INTERESES ",vtasa_oper USING "###.####","% ",
                                     vperiodo_int
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "REM*"
                    SELECT tasa_valor
                    INTO   vtasa_oper
                    FROM   tab_tasa_remanente
                    WHERE  tasa_origen = "VIV"
                    AND    tasa_fecha  = vfecha_valor

                    LET l_desc_mov = "INTERESES PAGO TRECE ",
                                     vtasa_oper USING "###.####","% ",vperiodo_int
                    LET l_id_aportante = "INFONAVIT"

               WHEN l_codigo MATCHES "RTR*"
                    LET vfecha_valor   = vfecha_valor - 1 UNITS MONTH
                    LET l_desc_mov     = "INTERESES (*) de ",
                                       vfecha_valor USING "mm/yyyy",
                                       " a ",vperiodo_int
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "UN*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
               WHEN l_codigo MATCHES "UC*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UM*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UQ*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UR-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "US-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UT-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UV-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UW-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UX-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UY-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UZ-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AL-*"
                    LET l_desc_mov = "CORRECCION "
                    LET l_id_aportante = razon_social CLIPPED

            WHEN l_codigo MATCHES "SALDO"
                 LET l_desc_mov = "PARTICIPACIONES VIVIENDA"
                 LET l_id_aportante = "INFONAVIT"
               OTHERWISE
                    SELECT max(rfc_patron)
                    INTO   vrfc
                    FROM   temp_rfc_patron01
                    WHERE  reg_patronal_imss = det_viv97.id_aportante
                    AND    folio_sua         = vfolio_sua

                    LET vperiodo_pago = ""
                    LET vbimestre     = ""

                    SELECT periodo_pago
                    INTO   vperiodo_pago
                    FROM   dis_det_aporte
                    WHERE  n_seguro          = w_aux.seguro
                    AND    folio             = det_viv97.folio
                    AND    folio_pago_sua    = vfolio_sua
                    AND    consec_reg_lote   = vconsecutivo
                    AND    reg_patronal_imss = det_viv97.id_aportante

                    IF vperiodo_pago IS NULL THEN
                       SELECT max(periodo_pago)
                       INTO   vperiodo_pago
                       FROM   dis_det_aporte
                       WHERE  n_seguro          = w_aux.seguro
                       AND    folio             = det_viv97.folio
                       AND    folio_pago_sua    = vfolio_sua
                       AND    reg_patronal_imss = det_viv97.id_aportante
                    END IF

                  LET vbimestre = mes_a_bim(vperiodo_pago[1,4],vperiodo_pago[5,6])

                         LET l_desc_mov     = "APORTACION DE VIVIENDA ",
                                              vbimestre CLIPPED," ",
                                              vfecha_valor USING "dd/mm/yyyy"
                         LET l_id_aportante = vrfc CLIPPED
            END CASE
         ELSE                                    # Monto negativo
            LET vcargos_viv97 = vcargos_viv97 + det_viv97.m_pesos

            CASE
               WHEN l_codigo MATCHES "ACR*"
                    LET l_desc_mov     = "CREDITO DE VIVIENDA"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "ACR-U*"
                    LET l_desc_mov     = "CREDITO DE VIVIENDA"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo = "DEV-INF"
                    LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "DEV. I*"
                    LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "PAG-EXC*"
                    LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo = "PAG-EXC-VOL"
                    LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                    LET l_id_aportante = "INFONAVIT"
               WHEN l_codigo MATCHES "TR*"
                    LET l_desc_mov  = "TRASPASO VIVIENDA"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "TC*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "RET*"
                    LET l_desc_mov     = "RETIRO VIVIENDA"
                    LET l_id_aportante = "TRABAJADOR"
               WHEN l_codigo MATCHES "UN*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UC*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UM*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UQ*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UR-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "US-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UT-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UV-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UW-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UX-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "UY-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "UZ-*"
                    LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                    SELECT afore_desc
                    INTO   l_id_aportante
                    FROM   tab_afore
                    WHERE  afore_cod = l_sufijo
               WHEN l_codigo MATCHES "AA*"
                    LET l_desc_mov  = "TRASPASO VIVIENDA"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AS*"
                    LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 97"
                    LET l_id_aportante = razon_social CLIPPED
               WHEN l_codigo MATCHES "AL-*"
                    LET l_desc_mov = "CORRECCION "
                    LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "AJUSTE"
                 LET l_desc_mov = "PARTICIPACIONES VIVIENDA"
                 LET l_id_aportante = "INFONAVIT"
            END CASE
         END IF

      LET det_viv97.id_aportante = l_id_aportante
      --LET det_viv97.desc_mov     = l_desc_mov
      LET det_viv97.desc_mov     = det_viv97.tipo_char
      --LET det_viv97.tipo_char    = ""

         LET i_cont_de_reg = i_cont_de_reg + 1
         LET i = i + 1

         OUTPUT TO REPORT VVIV_97(det_viv97.*)
         LET l_desc_mov     = ""
         LET l_id_aportante = ""
    END FOREACH

    LET x_cargo_viv97 = vcargos_viv97
    LET x_abono_viv97 = vabonos_viv97

    FINISH REPORT VVIV_97

    LET sed = "sed -e '/^$/d' ",VIV_97," > ",VIV97
    RUN sed

    START REPORT SUMVIV_97 TO SUM_VIV_97
        OUTPUT TO REPORT SUMVIV_97()
    FINISH REPORT SUMVIV_97

    LET sed = "sed -e '/^$/d' ",SUM_VIV_97," > ",SUMVIV97
    RUN sed

#---------------------------------------------- VIV ISSSTE
    LET vbimestre = " "
    LET vcargos_vivis = 0
    LET vabonos_vivis = 0
    INITIALIZE det_vivis TO NULL

    DECLARE cur_vivis CURSOR FOR
    SELECT a.fecha_conversion,
           b.subct_desc,
           a.id_aportante,
           ROUND(a.monto_en_pesos,2),
           a.subcuenta,
           a.siefore,
           a.tipo_movimiento,
           c.descripcion,
           a.folio,
           a.folio_sua,
           a.consecutivo_lote,
           a.fecha_valor
    FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
    WHERE  subcuenta   = 14
    AND    b.subct_cod = a.subcuenta
    AND    c.codigo    = a.tipo_movimiento
    ORDER  BY 1,12

    LET i = 1
    START REPORT VVIV_IS TO VIV_IS

    LET i_cont_de_reg = 0
    FOREACH cur_vivis INTO det_vivis.*,
                           vfolio_sua,
                           vconsecutivo,
                           vfecha_valor

        LET l_codigo  = det_vivis.id_aportante

        IF det_vivis.id_aportante MATCHES "??-*" THEN
            LET l_sufijo = det_vivis.id_aportante[4,6]
        END IF

        IF det_vivis.m_pesos >= 0 THEN
            LET vabonos_viv97 = vabonos_viv97 + det_vivis.m_pesos
            LET vperiodo_int = periodo_int(vfecha_valor)

            CASE
                WHEN l_codigo MATCHES "ACR*"
                     LET l_desc_mov     = "CREDITO DE VIVIENDA"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo = "ACR-ULT"
                     LET l_desc_mov     = "CREDITO DE VIVIENDA"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo = "DEV-INF"
                     LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "DEV. I*"
                     LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "PAG-EXC*"
                     LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo = "PAG-EXC-VOL"
                     LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "MQ*"
                     LET l_desc_mov  = "TRASPASO VIVIENDA"
                     SELECT desc_afore
                     INTO   l_id_aportante
                     FROM   tab_prestadora
                     WHERE  cod_afore = l_sufijo
                WHEN l_codigo MATCHES "MC*"
                     LET l_desc_mov  = "TRASPASO VIVIENDA"
                     SELECT desc_afore
                     INTO   l_id_aportante
                     FROM   tab_prestadora
                     WHERE  cod_afore = l_sufijo
                WHEN l_codigo MATCHES "MA*"
                     LET l_desc_mov  = "TRASPASO VIVIENDA"
                     SELECT desc_afore
                     INTO   l_id_aportante
                     FROM   tab_prestadora
                     WHERE  cod_afore = l_sufijo
                WHEN l_codigo MATCHES "MS*"
                     LET l_desc_mov  = "TRASPASO VIVIENDA"
                     SELECT desc_afore
                     INTO   l_id_aportante
                     FROM   tab_prestadora
                     WHERE  cod_afore = l_sufijo
                WHEN l_codigo MATCHES "AF*"
                     LET l_desc_mov  = "TRASPASO VIVIENDA"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "AC*"
                     LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 97"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "AA*"
                     LET l_desc_mov  = "TRASPASO VIVIENDA"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "AS*"
                     LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 97"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "IN*"
                     SELECT tasa_valor
                     INTO   vtasa_oper
                     FROM   tab_tasa_ordinaria
                     WHERE  tasa_origen = "VIV"
                     AND    tasa_fecha  = vfecha_valor

                 LET l_desc_mov = "INTERESES ",vtasa_oper USING "###.####","% ",
                                      vperiodo_int
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "REM*"
                     SELECT tasa_valor
                     INTO   vtasa_oper
                     FROM   tab_tasa_remanente
                     WHERE  tasa_origen = "VIV"
                     AND    tasa_fecha  = vfecha_valor

                     LET l_desc_mov = "INTERESES PAGO TRECE ",
                                      vtasa_oper USING "###.####","% ",
                                      vperiodo_int
                     LET l_id_aportante = "INFONAVIT"

                WHEN l_codigo MATCHES "RTR*"
                     LET vfecha_valor   = vfecha_valor - 1 UNITS MONTH
                     LET l_desc_mov     = "INTERESES (*) de ",
                                          vfecha_valor USING "mm/yyyy",
                                          " a ",vperiodo_int
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "UN*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UC*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UM*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "UQ*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "UR-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "US-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "UT-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UV-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UW-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UX-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UY-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "UZ-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "AL-*"
                     LET l_desc_mov = "CORRECCION "
                     LET l_id_aportante = razon_social CLIPPED
                OTHERWISE
                     SELECT max(rfc_patron)
                     INTO   vrfc
                     FROM   temp_rfc_patron01
                     WHERE  reg_patronal_imss = det_vivis.id_aportante
                     AND    folio_sua         = vfolio_sua

                     LET vperiodo_pago = ""
                     LET vbimestre     = ""

                     SELECT periodo_pago
                     INTO   vperiodo_pago
                     FROM   dis_det_aporte
                     WHERE  n_seguro          = w_aux.seguro
                     AND    folio             = det_vivis.folio
                     AND    folio_pago_sua    = vfolio_sua
                     AND    consec_reg_lote   = vconsecutivo
                     AND    reg_patronal_imss = det_vivis.id_aportante

                     IF vperiodo_pago IS NULL THEN
                          SELECT max(periodo_pago)
                          INTO   vperiodo_pago
                          FROM   dis_det_aporte
                          WHERE  n_seguro          = w_aux.seguro
                          AND    folio             = det_vivis.folio
                          AND    folio_pago_sua    = vfolio_sua
                          --AND    consec_reg_lote   = vconsecutivo
                          AND    reg_patronal_imss = det_vivis.id_aportante
                     END IF

               LET vbimestre = mes_a_bim(vperiodo_pago[1,4],vperiodo_pago[5,6])
                         LET l_desc_mov = "APORTACION DE VIVIENDA ",
                                           vbimestre CLIPPED," ",
                                           vfecha_valor USING "dd/mm/yyyy"
                         LET l_id_aportante = vrfc CLIPPED
            END CASE
        ELSE                                     # Monto negativo
            LET vcargos_viv97 = vcargos_viv97 + det_vivis.m_pesos

            CASE
                WHEN l_codigo MATCHES "ACR*"
                     LET l_desc_mov     = "CREDITO DE VIVIENDA"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "ACR-U*"
                     LET l_desc_mov     = "CREDITO DE VIVIENDA"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo = "DEV-INF"
                     LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "DEV. I*"
                     LET l_desc_mov     = "DEVOLUCION INFONAVIT"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "PAG-EXC*"
                     LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo = "PAG-EXC-VOL"
                     LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                     LET l_id_aportante = "INFONAVIT"
                WHEN l_codigo MATCHES "TR*"
                     LET l_desc_mov  = "TRASPASO VIVIENDA"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "TC*"
                     LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "RET*"
                     LET l_desc_mov     = "RETIRO VIVIENDA"
                     LET l_id_aportante = "TRABAJADOR"
                WHEN l_codigo MATCHES "UN*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UC*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UM*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "UQ*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "UR-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "US-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "UT-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UV-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UW-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UX-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "UY-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "UZ-*"
                     LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                     SELECT afore_desc
                     INTO   l_id_aportante
                     FROM   tab_afore
                     WHERE  afore_cod = l_sufijo
                WHEN l_codigo MATCHES "AA*"
                     LET l_desc_mov  = "TRASPASO VIVIENDA"
                     LET l_id_aportante = razon_social CLIPPED
                WHEN l_codigo MATCHES "AS*"
                     LET l_desc_mov  = "TRASPASO COMPLEMENTARIO VIVIENDA 97"
                     LET l_id_aportante = razon_social CLIPPED
            END CASE
        END IF


      LET det_vivis.id_aportante = l_id_aportante
      --LET det_vivis.desc_mov     = l_desc_mov
      LET det_vivis.desc_mov     = det_vivis.tipo_char
      --LET det_vivis.tipo_char    = ""

        LET i_cont_de_reg = i_cont_de_reg + 1
        LET i = i + 1

        OUTPUT TO REPORT VVIV_IS(det_vivis.*)
        LET l_desc_mov     = ""
        LET l_id_aportante = ""
    END FOREACH

    LET x_cargo_vivis = vcargos_vivis
    LET x_abono_vivis = vabonos_vivis

    FINISH REPORT VVIV_IS

    LET sed = "sed -e '/^$/d' ",VIV_IS," > ",VIVIS
    RUN sed

    START REPORT SUMVIV_IS TO SUM_VIV_IS
        OUTPUT TO REPORT SUMVIV_IS()
    FINISH REPORT SUMVIV_IS

    LET sed = "sed -e '/^$/d' ",SUM_VIV_IS," > ",SUMVIVIS
    RUN sed

END FUNCTION
############################################################
FUNCTION detalle_vol()

   DEFINE l_desc_mov	  CHAR(45),
          l_tipo_char	  CHAR(45),
          l_id_aportante  CHAR(40),
          l_codigo	  CHAR(10),
          xtipo_mov	  SMALLINT,
          l_sufijo	  CHAR(10),
          l_subct_desc	  CHAR(40)

   LET vcargos_vol = 0
   LET vabonos_vol = 0
   LET xtipo_mov   = 0
   INITIALIZE det_vol TO NULL

   DECLARE cur_vol CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          a.tipo_movimiento,
          c.descripcion,
          a.folio_sua,
          a.consecutivo_lote,
          a.tipo_movimiento
   FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta IN(3,10)
   AND    b.subct_cod = a.subcuenta
   AND    c.codigo    = a.tipo_movimiento
   ORDER  BY 1,11

   LET i = 1
   LET vflag = 1
   LET i_cont_de_reg = 0

   START REPORT VVOL TO VOL

   FOREACH cur_vol INTO det_vol.*,
                        vconsecutivo,
                        xtipo_mov

      LET l_codigo  = det_vol.id_aportante

      IF det_vol.id_aportante MATCHES "??-*" THEN
         LET l_sufijo = det_vol.id_aportante[4,6]
      END IF

      IF det_vol.m_pesos >= 0 THEN
         LET vabonos_vol = vabonos_vol + det_vol.m_pesos

         CASE
            WHEN l_codigo MATCHES "METLIFE"
                 LET l_desc_mov     = "COMISION POR SALDO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "MQ*"
                 LET l_desc_mov  = "APORTACION VOLUNTARIA"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MC*"
                 LET l_desc_mov  = "APORTACION VOLUNTARIA COMPLEMENTARIO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MA*"
                 LET l_desc_mov  = "APORTACION VOLUNTARIA"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MS*"
                 LET l_desc_mov  = "APORTACION VOLUNTARIA COMPLEMENTARIO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "AF*"
                 LET l_desc_mov  = "APORTACION VOLUNTARIA"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AC*"
                 LET l_desc_mov  = "APORTACION VOLUNTARIA COMPLEMENTARIO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AA*"
                 LET l_desc_mov  = "APORTACION VOLUNTARIA"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AS*"
                 LET l_desc_mov  = "APORTACION VOLUNTARIA COMPLEMENTARIO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "VE*"
                 LET l_sufijo       = det_vol.id_aportante[3,6]
                 LET l_desc_mov     = "APORTACION VOLUNTARIA"
                 LET l_id_aportante = "TRABAJADOR "
            WHEN l_codigo MATCHES "UN*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UC*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UM*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UQ*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UR-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "US-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UT-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UV-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UW-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UX-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UY-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UZ-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "PAG-EXC*"
                 LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "PAG-EXC-VOL"
                 LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "BANXICO"
                 LET l_desc_mov    = "INTERESES APORTACIONES VOLUNTARIAS"
                 LET l_id_aportante = "GOBIERNO FEDERAL"
            WHEN l_codigo MATCHES "AL-*"
                 LET l_desc_mov = "CORRECCION "
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TDA-*"
                 LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
                 LET   l_id_aportante = "SB1"
            WHEN l_codigo MATCHES "SOLORDSEL"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
            WHEN l_codigo MATCHES "DECIMOS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
            WHEN l_codigo MATCHES "CORTE"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "CORTE TRANSVERSAL"
            WHEN l_codigo MATCHES "INDEBIDO"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA"
            WHEN l_codigo MATCHES "INDIMSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
            WHEN l_codigo MATCHES "INDISSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
            WHEN l_codigo MATCHES "INDISSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
            WHEN l_codigo MATCHES "INDUNIF"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
            WHEN l_codigo MATCHES "INDTRAP"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
            WHEN l_codigo MATCHES "SOLASIG"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
            OTHERWISE
                 SELECT max(rfc_patron)
                 INTO   vrfc
                 FROM   temp_rfc_patron01
                 WHERE  reg_patronal_imss = det_vol.id_aportante
                 AND    folio_sua         = det_vol.folio

                 LET l_desc_mov     = "APORTACION VOLUNTARIA EN SUA"
                 LET l_id_aportante = vrfc CLIPPED
         END CASE
      ELSE                              # Monto negativo
         LET vcargos_vol = vcargos_vol + det_vol.m_pesos

         CASE
            WHEN l_codigo MATCHES "METLIFE"
                 LET l_desc_mov     = "COMISION POR SALDO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TR*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES VOLUNTARIAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TC*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES VOLUNTARIAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "AA*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES VOLUNTARIAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "AS*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES VOLUNTARIAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "PAG-EXC*"
                 LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "PAG-EXC-VOL"
                 LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo MATCHES "RET*"
                 CASE xtipo_mov
                    WHEN 10
                         LET l_desc_mov     = "IMPUESTOS RETENIDOS"
                         LET l_id_aportante = "GOBIERNO FEDERAL"
                    OTHERWISE
                         LET l_desc_mov     = "RETIRO APORTACION VOLUNTARIA"
                         LET l_id_aportante = "TRABAJADOR"
                 END CASE
            WHEN l_codigo MATCHES "UN*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UC*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UM*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UQ*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UR-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "US-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UT-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UV-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UW-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UX-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UY-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UZ-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AL-*"
                 LET l_desc_mov = "CORRECCION "
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TDA-*"
                 LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
                 LET   l_id_aportante = "SB1"
            WHEN l_codigo MATCHES "SOLORDSEL"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
            WHEN l_codigo MATCHES "DECIMOS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
            WHEN l_codigo MATCHES "CORTE"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "CORTE TRANSVERSAL"
            WHEN l_codigo MATCHES "INDEBIDO"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA"
            WHEN l_codigo MATCHES "INDIMSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
            WHEN l_codigo MATCHES "INDISSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
            WHEN l_codigo MATCHES "INDUNIF"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
            WHEN l_codigo MATCHES "INDTRAP"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
            WHEN l_codigo MATCHES "SOLASIG"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
            OTHERWISE
                 LET l_desc_mov = "BONIFICACION APORTACIONES VOLUNTARIAS"
                  LET l_id_aportante = razon_social CLIPPED
         END CASE
      END IF

      LET det_vol.id_aportante = l_id_aportante
      --LET det_vol.desc_mov     = l_desc_mov
      LET det_vol.desc_mov     = det_vol.tipo_char
      --LET det_vol.tipo_char    = ""

      LET i_cont_de_reg = i_cont_de_reg + 1
      LET i = i + 1
      OUTPUT TO REPORT VVOL(det_vol.*)
      LET  l_desc_mov     = ""
      LET  l_id_aportante = ""
   END FOREACH

   LET x_cargo_vol = vcargos_vol
   LET x_abono_vol = vabonos_vol

   FINISH REPORT VVOL

   LET sed = "sed -e '/^$/d' ",VOL," > ",VOLL
   RUN sed

   START REPORT SSUM_VOL TO SUM_VOL
      OUTPUT TO REPORT SSUM_VOL()
   FINISH REPORT SSUM_VOL

   LET sed = "sed -e '/^$/d' ",SUM_VOL," > ",SUMVOL
   RUN sed

END FUNCTION
############################################################
FUNCTION detalle_ahorro()

   DEFINE l_desc_mov         CHAR(45),
          l_tipo_char	     CHAR(45),
          l_id_aportante     CHAR(40),
          l_codigo	     CHAR(10),
          xtipo_mov	     SMALLINT,
          l_sufijo	     CHAR(10),
          l_subct_desc	     CHAR(40)

   LET vcargos_vol = 0
   LET vabonos_vol = 0
   LET xtipo_mov   = 0
   INITIALIZE det_vol TO NULL

   DECLARE cur_ahorro CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          a.tipo_movimiento,
          c.descripcion,
          a.folio_sua,
          a.consecutivo_lote,
          a.tipo_movimiento
   FROM   tmp_cuenta a,tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta = 15
   AND    b.subct_cod = a.subcuenta
   AND    c.codigo = a.tipo_movimiento
   ORDER  BY 1,11

   LET i = 1
   LET vflag = 1
   LET i_cont_de_reg = 0

   START REPORT RAHORRO TO AHORRO

   FOREACH cur_ahorro INTO det_vol.*,
                           vconsecutivo,
                           xtipo_mov

      LET l_codigo  = det_vol.id_aportante

      IF det_vol.id_aportante MATCHES "??-*" THEN
         LET l_sufijo = det_vol.id_aportante[4,6]
      END IF

      IF det_vol.m_pesos >= 0 THEN
         CASE
            WHEN l_codigo MATCHES "METLIFE"
                 LET l_desc_mov     = "COMISION POR SALDO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "MQ*"
                 LET l_desc_mov  = "APORTACION A LARGO PLAZO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MC*"
                 LET l_desc_mov  = "APORTACION ALP COMPLEMENTARIO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MA*"
                 LET l_desc_mov  = "APORTACION A LARGO PLAZO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MS*"
                 LET l_desc_mov  = "APORTACION ALP COMPLEMENTARIO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "AF*"
                 LET l_desc_mov  = "APORTACION A LARGO PLAZO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AC*"
                 LET l_desc_mov  = "APORTACION ALP COMPLEMENTARIO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AA*"
                 LET l_desc_mov  = "APORTACION A LARGO PLAZO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AS*"
                 LET l_desc_mov  = "APORTACION ALP COMPLEMENTARIO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "VE*"
                 LET l_sufijo       = det_vol.id_aportante[3,6]
                 LET l_desc_mov     = "APORTACION A LARGO PLAZO"
                 LET l_id_aportante = "TRABAJADOR "
            WHEN l_codigo MATCHES "UN*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UC*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UM*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UQ*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UR-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "US-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UT-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UV-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UW-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UX-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UY-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UZ-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "PAG-EXC*"
                 LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "PAG-EXC-VOL"
                 LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "BANXICO"
                 LET l_desc_mov    = "INTERESES APORTACIONES ALP"
                 LET l_id_aportante = "GOBIERNO FEDERAL"
            WHEN l_codigo MATCHES "AL-*"
                 LET l_desc_mov = "CORRECCION "
                 LET l_id_aportante = razon_social CLIPPED
            OTHERWISE
                 SELECT max(rfc_patron)
                 INTO   vrfc
                 FROM   temp_rfc_patron01
                 WHERE  reg_patronal_imss = det_vol.id_aportante
                 AND    folio_sua         = det_vol.folio

                 LET l_desc_mov     = "APORTACION A LARGO PLAZO VIA SUA"
                 LET l_id_aportante = vrfc CLIPPED
         END CASE
      ELSE                                  # Monto negativo
         LET vcargos_vol = vcargos_vol + det_vol.m_pesos

         CASE
            WHEN l_codigo MATCHES "METLIFE"
                 LET l_desc_mov     = "COMISION POR SALDO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TR*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES ALP"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TC*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES ALP"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "AA*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES ALP"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "AS*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES ALP"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "PAG-EXC*"
                 LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "PAG-EXC-VOL"
                 LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo MATCHES "RET*"
                 CASE xtipo_mov
                    WHEN 10
                         LET l_desc_mov     = "IMPUESTOS RETENIDOS"
                         LET l_id_aportante = "GOBIERNO FEDERAL"
                    OTHERWISE
                         LET l_desc_mov     = "RETIRO APORTACION ALP"
                         LET l_id_aportante = "TRABAJADOR"
                 END CASE
            WHEN l_codigo MATCHES "UN*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UC*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UM*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UQ*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UR-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "US-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UT-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UV-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UW-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UX-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UY-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UZ-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AL-*"
                 LET l_desc_mov = "CORRECCION "
                 LET l_id_aportante = razon_social CLIPPED
            OTHERWISE
                 LET l_desc_mov = "BONIFICACION APORTACIONES ALP"
                  LET l_id_aportante = razon_social CLIPPED
         END CASE
      END IF

      LET det_vol.id_aportante = l_id_aportante
      --LET det_vol.desc_mov     = l_desc_mov
      LET det_vol.desc_mov     = det_vol.tipo_char
      --LET det_vol.tipo_char    = ""

      LET i_cont_de_reg = i_cont_de_reg + 1
      LET i = i + 1
      OUTPUT TO REPORT RAHORRO(det_vol.*)
      LET  l_desc_mov     = ""
      LET  l_id_aportante = ""
   END FOREACH

   LET x_cargo_aho = vcargos_vol
   LET x_abono_aho = vabonos_vol

   FINISH REPORT RAHORRO

   LET sed = "sed -e '/^$/d' ",AHORRO," > ",VAHORRO
   RUN sed

   START REPORT SSUM_AHO TO SUM_AHORRO
      OUTPUT TO REPORT SSUM_AHO()
   FINISH REPORT SSUM_AHO

   LET sed = "sed -e '/^$/d' ",SUM_AHORRO," > ",SUMAHORRO
   RUN sed

END FUNCTION
############################################################
FUNCTION detalle_retiro()

   DEFINE l_desc_mov		  CHAR(45),
          l_tipo_char		  CHAR(45),
          l_id_aportante	CHAR(40),
          l_codigo		    CHAR(10),
          xtipo_mov	      SMALLINT,
          l_sufijo		    CHAR(10),
          l_subct_desc		CHAR(40)

   LET vcargos_vol = 0
   LET vabonos_vol = 0
   LET xtipo_mov   = 0
   INITIALIZE det_vol TO NULL

   DECLARE cur_retiro CURSOR FOR
   SELECT a.fecha_conversion,
          b.subct_desc,
          a.id_aportante,
          ROUND(a.monto_en_pesos,2),
          a.subcuenta,
          a.siefore,
          a.tipo_movimiento,
          c.descripcion,
          a.folio_sua,
          a.consecutivo_lote,
          a.tipo_movimiento
   FROM   tmp_cuenta a, tab_subcuenta b,tab_movimiento c
   WHERE  subcuenta IN(11,12)
   AND    b.subct_cod = a.subcuenta
   AND    c.codigo    = a.tipo_movimiento
   ORDER  BY 1,11

   LET i = 1
   LET vflag = 1

   START REPORT RRET TO RETIRO
       LET i_cont_de_reg = 0

   FOREACH cur_retiro INTO det_vol.*,
                           vconsecutivo,
                           xtipo_mov

      LET l_codigo  = det_vol.id_aportante

      IF det_vol.id_aportante MATCHES "??-*" THEN
         LET l_sufijo = det_vol.id_aportante[4,6]
      END IF

      IF det_vol.m_pesos >= 0 THEN
         LET vabonos_vol = vabonos_vol + det_vol.m_pesos

         CASE
            WHEN l_codigo MATCHES "METLIFE"
                 LET l_desc_mov     = "COMISION POR SALDO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "MQ*"
                 LET l_desc_mov  = "APORTACION COMP. RETIRO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MC*"
                 LET l_desc_mov  = "APORTACION COMP. RETIRO COMPLEMENTARIO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MA*"
                 LET l_desc_mov  = "APORTACION COMP. RETIRO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "MS*"
                 LET l_desc_mov  = "APORTACION COMP. RETIRO COMPLEMENTARIO"
                 SELECT desc_afore
                 INTO   l_id_aportante
                 FROM   tab_prestadora
                 WHERE  cod_afore = l_sufijo
            WHEN l_codigo MATCHES "AF*"
                 LET l_desc_mov  = "APORTACION COMP. RETIRO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AC*"
                 LET l_desc_mov  = "APORTACION COMP. RETIRO COMPLEMENTARIO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AA*"
                 LET l_desc_mov  = "APORTACION COMP. RETIRO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AS*"
                 LET l_desc_mov  = "APORTACION COMP. RETIRO COMPLEMENTARIO"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "VE*"
                 LET l_sufijo       = det_vol.id_aportante[3,6]
                 LET l_desc_mov     = "APORTACION COMP. RETIRO"
                 LET l_id_aportante = "TRABAJADOR "
            WHEN l_codigo MATCHES "UN*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UC*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UM*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UQ*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UR-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "US-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UT-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UV-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UW-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UX-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UY-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UZ-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "PAG-EXC*"
                 LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "PAG-EXC-VOL"
                 LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "BANXICO"
                 LET l_desc_mov    = "INTERESES APORTACIONES COMP. RETIRO"
                 LET l_id_aportante = "GOBIERNO FEDERAL"
            WHEN l_codigo MATCHES "AL-*"
                 LET l_desc_mov = "CORRECCION "
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TDA-*"
                 LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
                 LET   l_id_aportante = "SB1"
            WHEN l_codigo MATCHES "SOLORDSEL"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
            WHEN l_codigo MATCHES "DECIMOS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
            WHEN l_codigo MATCHES "CORTE"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "CORTE TRANSVERSAL"
            WHEN l_codigo MATCHES "INDEBIDO"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA"
            WHEN l_codigo MATCHES "INDIMSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
            WHEN l_codigo MATCHES "INDISSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
            WHEN l_codigo MATCHES "INDUNIF"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
            WHEN l_codigo MATCHES "INDTRAP"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
            WHEN l_codigo MATCHES "SOLASIG"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
            OTHERWISE
                 SELECT max(rfc_patron)
                 INTO   vrfc
                 FROM   temp_rfc_patron01
                 WHERE  reg_patronal_imss = det_vol.id_aportante
                 AND    folio_sua         = det_vol.folio

                 LET l_desc_mov     = "APORTACION COMP. RETIRO VIA SUA"
                 LET l_id_aportante = vrfc CLIPPED
         END CASE
      ELSE                                  # Monto negativo
         LET vcargos_vol = vcargos_vol + det_vol.m_pesos

         CASE
            WHEN l_codigo MATCHES "METLIFE"
                 LET l_desc_mov     = "COMISION POR SALDO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TR*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES COMP. RETIRO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TC*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES COMP. RETIRO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "AA*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES COMP. RETIRO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "AS*"
                 LET l_desc_mov     = "TRASPASO APORTACIONES COMP. RETIRO"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "PAG-EXC*"
                 LET l_desc_mov     = "DEVOLUCION PAGOS EN EXCESO"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo = "PAG-EXC-VOL"
                 LET l_desc_mov   = "DEVOLUCION PAGOS EN EXCESO TRABAJADOR"
                 LET l_id_aportante = "IMSS"
            WHEN l_codigo MATCHES "RET*"
                 CASE xtipo_mov
                    WHEN 10
                         LET l_desc_mov     = "IMPUESTOS RETENIDOS"
                         LET l_id_aportante = "GOBIERNO FEDERAL"
                    OTHERWISE
                         LET l_desc_mov     = "RETIRO APORTACION COMP. RETIRO"
                         LET l_id_aportante = "TRABAJADOR"
                 END CASE
            WHEN l_codigo MATCHES "UN*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UC*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UM*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UQ*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UR-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "US-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UT-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UV-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UW-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UX-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "UY-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "UZ-*"
                 LET l_desc_mov     = "UNIFICACION DE CUENTAS"
                 SELECT afore_desc
                 INTO   l_id_aportante
                 FROM   tab_afore
                 WHERE  afore_cod = l_sufijo
            WHEN l_codigo MATCHES "AL-*"
                 LET l_desc_mov = "CORRECCION "
                 LET l_id_aportante = razon_social CLIPPED
            WHEN l_codigo MATCHES "TDA-*"
                 LET l_desc_mov     = "TRANSFERENCIA MAYOR 56"
                 LET   l_id_aportante = "SB1"
            WHEN l_codigo MATCHES "SOLORDSEL"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "SOLICITUD POR ORDEN DE SELECCION"
            WHEN l_codigo MATCHES "DECIMOS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "SOLICITUD COMPLETA DECIMOS"
            WHEN l_codigo MATCHES "CORTE"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "CORTE TRANSVERSAL"
            WHEN l_codigo MATCHES "INDEBIDO"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA"
            WHEN l_codigo MATCHES "INDIMSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-IMSS"
            WHEN l_codigo MATCHES "INDISSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
            WHEN l_codigo MATCHES "INDISSS"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA ICEFA-ISSSTE"
            WHEN l_codigo MATCHES "INDUNIF"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA UNIFICACION"
            WHEN l_codigo MATCHES "INDTRAP"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "LIQUIDACION INDEBIDA TRASPASO"
            WHEN l_codigo MATCHES "SOLASIG"
                 LET l_desc_mov     = "TRASPASO ENTRE SIEFORES"
                 LET l_id_aportante = "ASIGNADO CERTIFICADO MENOR 56"
            OTHERWISE
                 LET l_desc_mov = "BONIFICACION APORTACIONES COMP. RETIRO"
                 LET l_id_aportante = razon_social CLIPPED
         END CASE
      END IF

      LET det_vol.id_aportante = l_id_aportante
      --LET det_vol.desc_mov     = l_desc_mov
      LET det_vol.desc_mov     = det_vol.tipo_char
      --LET det_vol.tipo_char    = ""

      LET i_cont_de_reg = i_cont_de_reg + 1
      LET i = i + 1
      OUTPUT TO REPORT RRET(det_vol.*)
      LET  l_desc_mov     = ""
      LET  l_id_aportante = ""
   END FOREACH

   LET x_cargo_ret = vcargos_vol
   LET x_abono_ret = vabonos_vol

   FINISH REPORT RRET

   LET sed = "sed -e '/^$/d' ",RETIRO," > ",VRETIRO
   RUN sed

   START REPORT SSUM_RET TO SUM_RETIRO
      OUTPUT TO REPORT SSUM_RET()
   FINISH REPORT SSUM_RET

   LET sed = "sed -e '/^$/d' ",SUM_RETIRO," > ",SUMRETIRO
   RUN sed

END FUNCTION
############################################################
FUNCTION sumario()

       START REPORT SSUM_DIS TO SUM_DIS
           OUTPUT TO REPORT SSUM_DIS()
       FINISH REPORT SSUM_DIS

       LET sed = "sed -e '/^$/d' ",SUM_DIS," > ",SUMDIS
       RUN sed

END FUNCTION
############################################################
-----------------------------------------Reporte detalle SAR
REPORT SAR922(det_sar)

    DEFINE det_sar RECORD #loc #det_sar
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_char             CHAR(29)
    END RECORD

    OUTPUT
       PAGE LENGTH 60
	     LEFT MARGIN 0
	     RIGHT MARGIN 0
	     TOP MARGIN 0
	     BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        IF det_sar.m_pesos < 0 THEN
            PRINT
                COLUMN 001,"03" ,
                COLUMN 003,det_sar.fecha USING "DDMMYYYY",
                COLUMN 011,det_sar.siefore_cod USING "&&",
                COLUMN 013,det_sar.desc_mov,
                COLUMN 058,det_sar.id_aportante CLIPPED,
                COLUMN 088,-det_sar.m_pesos USING "#######&.&&",
                COLUMN 099,"       0.00"
        ELSE
            PRINT
                COLUMN 001,"03",
                COLUMN 003,det_sar.fecha USING "DDMMYYYY",
                COLUMN 011,det_sar.siefore_cod USING "&&",
                COLUMN 013,det_sar.desc_mov,
                COLUMN 058,det_sar.id_aportante CLIPPED,
                COLUMN 088,"       0.00",
                COLUMN 099,det_sar.m_pesos USING "#######&.&&"
        END IF
END REPORT
############################################################
-----------------------------------------Reporte sumario SAR
REPORT SUMSAR_92()
#SS92--------------
    OUTPUT
        PAGE LENGTH 2
	     LEFT MARGIN 0
	     RIGHT MARGIN 0
	     TOP MARGIN 0
	     BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL SAR ANTERIOR#

        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"03" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_sar   USING "#######&.&&",
            COLUMN 27,vabonos_sar   USING "#######&.&&"
END REPORT
############################################################
-----------------------------------Reporte detalle SARISSSTE
REPORT SAR92ISSSTE(det_sar)

    DEFINE det_sar RECORD #loc #det_sar
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        --tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER,
        folio_sua             CHAR(6)
    END RECORD

    OUTPUT
       PAGE LENGTH 60
	     LEFT MARGIN 0
	     RIGHT MARGIN 0
	     TOP MARGIN 0
	     BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        IF det_sar.m_pesos < 0 THEN
            PRINT
                COLUMN 001,"03" ,
                COLUMN 003,det_sar.fecha USING "DDMMYYYY",
                COLUMN 011,det_sar.siefore_cod USING "&&",
                COLUMN 013,det_sar.desc_mov,
                COLUMN 058,det_sar.id_aportante CLIPPED,
                COLUMN 088,-det_sar.m_pesos USING "#######&.&&",
                COLUMN 099,"       0.00"
        ELSE
            PRINT
                COLUMN 001,"04",
                COLUMN 003,det_sar.fecha USING "DDMMYYYY",
                COLUMN 011,det_sar.siefore_cod USING "&&",
                COLUMN 013,det_sar.desc_mov,
                COLUMN 058,det_sar.id_aportante CLIPPED,
                COLUMN 088,"       0.00",
                COLUMN 099,det_sar.m_pesos USING "#######&.&&"
        END IF
END REPORT
############################################################
-----------------------------------Reporte sumario SARISSSTE
REPORT SUMSAR_92_ISSSTE()
#SS92--------------
    OUTPUT
       PAGE LENGTH 2
	     LEFT MARGIN 0
	     RIGHT MARGIN 0
	     TOP MARGIN 0
	     BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL SAR ANTERIOR#

        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"04" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_sar   USING "#######&.&&",
            COLUMN 27,vabonos_sar   USING "#######&.&&"
END REPORT
############################################################
-----------------------------------------Reporte detalle RCV
REPORT RRCV(det_rcv)

    DEFINE det_rcv RECORD #glo #det_rcv
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH 60
	     LEFT MARGIN 0
	     RIGHT MARGIN 0
	     TOP MARGIN 0
	     BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            IF det_rcv.m_pesos < 0 THEN
                PRINT
                    COLUMN 001,"05",
                    COLUMN 003,det_rcv.fecha USING "DDMMYYYY",
                    COLUMN 011,det_rcv.siefore_cod USING "&&",
                    COLUMN 013,det_rcv.desc_mov CLIPPED,
                    COLUMN 058,det_rcv.id_aportante CLIPPED,
                    COLUMN 088,-det_rcv.m_pesos USING "#######&.&&" ,
                    COLUMN 099,"       0.00"
            ELSE
                PRINT
                    COLUMN 001,"05" ,
                    COLUMN 003,det_rcv.fecha USING "DDMMYYYY",
                    COLUMN 011,det_rcv.siefore_cod USING "&&",
                    COLUMN 013,det_rcv.desc_mov CLIPPED,
                    COLUMN 058,det_rcv.id_aportante  CLIPPED,
                    COLUMN 088,"       0.00" ,
                    COLUMN 099,det_rcv.m_pesos USING "#######&.&&"
            END IF
END REPORT
############################################################
-----------------------------------------Reporte Sumario RCV
REPORT SSUM_RCV()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL RCV#
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"05" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_rcv   USING "#######&.&&",
            COLUMN 27,vabonos_rcv   USING "#######&.&&"
END REPORT
############################################################
-----------------------------------------Reporte detalle CV
REPORT RECV(det_cv)

    DEFINE det_cv RECORD #glo #det_rcv
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH 60
             LEFT MARGIN 0
             RIGHT MARGIN 0
             TOP MARGIN 0
             BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            IF det_cv.m_pesos < 0 THEN
                PRINT
                    COLUMN 001,"06",
                    COLUMN 003,det_cv.fecha USING "DDMMYYYY",
                    COLUMN 011,det_cv.siefore_cod USING "&&",
                    COLUMN 013,det_cv.tipo_char CLIPPED,
                    COLUMN 058,det_cv.id_aportante CLIPPED,
                    COLUMN 088,-det_cv.m_pesos USING "#######&.&&" ,
                    COLUMN 099,"       0.00"
            ELSE
                PRINT
                    COLUMN 001,"06" ,
                    COLUMN 003,det_cv.fecha USING "DDMMYYYY",
                    COLUMN 011,det_cv.siefore_cod USING "&&",
                    COLUMN 013,det_cv.tipo_char CLIPPED,
                    COLUMN 058,det_cv.id_aportante  CLIPPED,
                    COLUMN 088,"       0.00" ,
                    COLUMN 099,det_cv.m_pesos USING "#######&.&&"
            END IF
END REPORT
############################################################
-----------------------------------------Reporte Sumario CV
REPORT SSUM_CV()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL CV#
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"06" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_rcv   USING "#######&.&&",
            COLUMN 27,vabonos_rcv   USING "#######&.&&"
END REPORT
############################################################
-----------------------------------------Reporte detalle CS
REPORT RECS(det_cs)

    DEFINE det_cs RECORD #glo #det_rcv
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH 60
             LEFT MARGIN 0
             RIGHT MARGIN 0
             TOP MARGIN 0
             BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            IF det_cs.m_pesos < 0 THEN
                PRINT
                    COLUMN 001,"07",
                    COLUMN 003,det_cs.fecha USING "DDMMYYYY",
                    COLUMN 011,det_cs.siefore_cod USING "&&",
                    COLUMN 013,det_cs.tipo_char CLIPPED,
                    COLUMN 058,det_cs.id_aportante CLIPPED,
                    COLUMN 088,-det_cs.m_pesos USING "#######&.&&" ,
                    COLUMN 099,"       0.00"
            ELSE
                PRINT
                    COLUMN 001,"07",
                    COLUMN 003,det_cs.fecha USING "DDMMYYYY",
                    COLUMN 011,det_cs.siefore_cod USING "&&",
                    COLUMN 013,det_cs.tipo_char CLIPPED,
                    COLUMN 058,det_cs.id_aportante  CLIPPED,
                    COLUMN 088,"       0.00" ,
                    COLUMN 099,det_cs.m_pesos USING "#######&.&&"
            END IF
END REPORT
############################################################
-----------------------------------------Reporte Sumario CS
REPORT SSUM_CS()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL CS#
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"07" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_rcv   USING "#######&.&&",
            COLUMN 27,vabonos_rcv   USING "#######&.&&"
END REPORT
############################################################
############################################################
--------------------------------------Reporte detalle Viv 92
REPORT VVIV_92(det_viv92)

    DEFINE det_viv92 RECORD #loc #det_viv92
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
       PAGE LENGTH   60
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
        IF det_viv92.m_pesos < 0 THEN
            PRINT
                COLUMN 001,"08" ,
                COLUMN 003,det_viv92.fecha USING "DDMMYYYY",
                COLUMN 011,det_viv92.siefore_cod USING "&&",
                COLUMN 013,det_viv92.desc_mov CLIPPED,
                COLUMN 058,det_viv92.id_aportante CLIPPED,
                COLUMN 088,-det_viv92.m_pesos USING "#######&.&&",
                COLUMN 090,"       0.00"
        ELSE
            PRINT
                COLUMN 001,"08" ,
                COLUMN 003,det_viv92.fecha USING "DDMMYYYY",
                COLUMN 011,det_viv92.siefore_cod USING "&&",
                COLUMN 013,det_viv92.desc_mov CLIPPED,
                COLUMN 058,det_viv92.id_aportante  CLIPPED,
                COLUMN 088,"       0.00",
                COLUMN 099,det_viv92.m_pesos USING "#######&.&&"
        END IF
END REPORT
############################################################
-----------------------------------------Sumario Vivienda 92
REPORT SUMVIV_92()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL VIVIENDA 92#
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"08" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_viv92 USING "#######&.&&",
            COLUMN 27,vabonos_viv92 USING "#######&.&&"
END REPORT
############################################################
--------------------------------------Reporte detalle Viv 97
REPORT VVIV_97(det_viv97)

    DEFINE det_viv97 RECORD #loc #det_viv97
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
       PAGE LENGTH  60
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        IF det_viv97.m_pesos < 0 THEN
            PRINT
                COLUMN 001,"09" ,
                COLUMN 003,det_viv97.fecha USING "DDMMYYYY",
                COLUMN 011,det_viv97.siefore_cod USING "&&",
                COLUMN 013,det_viv97.desc_mov CLIPPED,
                COLUMN 058,det_viv97.id_aportante CLIPPED,
                COLUMN 088,-det_viv97.m_pesos USING "#######&.&&" ,
                COLUMN 099,"       0.00"
        ELSE
            PRINT
                COLUMN 001,"09" ,
                COLUMN 003,det_viv97.fecha USING "DDMMYYYY",
                COLUMN 011,det_viv97.siefore_cod USING "&&",
                COLUMN 013,det_viv97.desc_mov CLIPPED,
                COLUMN 058,det_viv97.id_aportante  CLIPPED,
                COLUMN 088,"       0.00" ,
                COLUMN 099,det_viv97.m_pesos USING "#######&.&&"
        END IF
END REPORT
############################################################
----------------------------------------------Sumario Viv 97
REPORT SUMVIV_97()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL VIVIENDA 97#
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"09" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_viv97 USING "#######&.&&",
            COLUMN 27,vabonos_viv97 USING "#######&.&&"
END REPORT
############################################################
---------------------------------Reporte detalle issste
REPORT VVIV_IS(det_vol)

    DEFINE det_vol RECORD #loc #det_vol
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
       PAGE LENGTH   60
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
        IF det_vol.m_pesos < 0 THEN
            PRINT
                COLUMN 001,"10" ,
                COLUMN 003,det_vol.fecha USING "DDMMYYYY",
                COLUMN 011,det_vol.siefore_cod USING "&&",
                COLUMN 013,det_vol.desc_mov CLIPPED,
                COLUMN 058,det_vol.id_aportante CLIPPED,
                COLUMN 088,-det_vol.m_pesos USING "#######&.&&",
                COLUMN 099,"       0.00"
        ELSE
            PRINT
                COLUMN 001,"10" ,
                COLUMN 003,det_vol.fecha USING "DDMMYYYY",
                COLUMN 011,det_vol.siefore_cod USING "&&",
                COLUMN 013,det_vol.desc_mov CLIPPED,
                COLUMN 058,det_vol.id_aportante  CLIPPED,
                COLUMN 088,"       0.00",
                COLUMN 099,det_vol.m_pesos USING "#######&.&&"
        END IF
END REPORT
############################################################
------------------------------------------Sumario Viv ISSSTE
REPORT SUMVIV_IS()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL VOLUNTARIAS#
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"10" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_vivis   USING "#######&.&&",
            COLUMN 27,vabonos_vivis   USING "#######&.&&"
END REPORT
############################################################
---------------------------------Reporte detalle Voluntarias
REPORT VVOL(det_vol)

    DEFINE det_vol RECORD #loc #det_vol
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH  60
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
        IF det_vol.m_pesos < 0 THEN
            PRINT
                COLUMN 001,"11" ,
                COLUMN 003,det_vol.fecha USING "DDMMYYYY",
                COLUMN 011,det_vol.siefore_cod USING "&&",
                COLUMN 013,det_vol.desc_mov CLIPPED,
                COLUMN 058,det_vol.id_aportante CLIPPED,
                COLUMN 088,-det_vol.m_pesos USING "#######&.&&",
                COLUMN 099,"       0.00"
        ELSE
            PRINT
                COLUMN 001,"11" ,
                COLUMN 003,det_vol.fecha USING "DDMMYYYY",
                COLUMN 011,det_vol.siefore_cod USING "&&",
                COLUMN 013,det_vol.desc_mov CLIPPED,
                COLUMN 058,det_vol.id_aportante  CLIPPED,
                COLUMN 088,"       0.00",
                COLUMN 099,det_vol.m_pesos USING "#######&.&&"
        END IF
END REPORT
############################################################
-----------------------------------------Sumario Voluntarias
REPORT SSUM_VOL()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL VOLUNTARIAS#
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"11" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_vol   USING "#######&.&&",
            COLUMN 27,vabonos_vol   USING "#######&.&&"
END REPORT
############################################################
-------------------------------------Reporte detalle Retiros
REPORT RRET(det_vol)

    DEFINE det_vol RECORD #loc #det_vol
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
       PAGE LENGTH   60
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
        IF det_vol.m_pesos < 0 THEN
            PRINT
                COLUMN 001,"12" ,
                COLUMN 003,det_vol.fecha USING "DDMMYYYY",
                COLUMN 011,det_vol.siefore_cod USING "&&",
                COLUMN 013,det_vol.desc_mov CLIPPED,
                COLUMN 058,det_vol.id_aportante CLIPPED,
                COLUMN 088,-det_vol.m_pesos USING "#######&.&&",
                COLUMN 099,"       0.00"
        ELSE
            PRINT
                COLUMN 001,"12" ,
                COLUMN 003,det_vol.fecha USING "DDMMYYYY",
                COLUMN 011,det_vol.siefore_cod USING "&&",
                COLUMN 013,det_vol.desc_mov CLIPPED,
                COLUMN 058,det_vol.id_aportante  CLIPPED,
                COLUMN 088,"       0.00",
                COLUMN 099,det_vol.m_pesos USING "#######&.&&"
        END IF
END REPORT
############################################################
---------------------------------------------Sumario Retiros
REPORT SSUM_RET()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL RETIROS
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"12" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_vol   USING "#######&.&&",
            COLUMN 27,vabonos_vol   USING "#######&.&&"
END REPORT
############################################################
--------------------------------------Reporte detalle Ahorro
REPORT RAHORRO(det_vol) --arc igual que el anterior

    DEFINE det_vol RECORD #loc #det_vol
        fecha                 DATE,
        desc_mov              CHAR(45),
        id_aportante          CHAR(30),
        m_pesos               DECIMAL(10,2),
        subcuenta             SMALLINT,
        siefore_cod           SMALLINT,
        tipo_movimiento       SMALLINT,
        tipo_char             CHAR(29),
        folio                 INTEGER
    END RECORD

    OUTPUT
       PAGE LENGTH   60
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
        IF det_vol.m_pesos < 0 THEN
            PRINT
                COLUMN 001,"13" ,
                COLUMN 003,det_vol.fecha USING "DDMMYYYY",
                COLUMN 011,det_vol.siefore_cod USING "&&",
                COLUMN 013,det_vol.desc_mov CLIPPED,
                COLUMN 058,det_vol.id_aportante CLIPPED,
                COLUMN 088,-det_vol.m_pesos USING "#######&.&&",
                COLUMN 099,"       0.00"
        ELSE
            PRINT
                COLUMN 001,"13" ,
                COLUMN 003,det_vol.fecha USING "DDMMYYYY",
                COLUMN 011,det_vol.siefore_cod USING "&&",
                COLUMN 013,det_vol.desc_mov CLIPPED,
                COLUMN 058,det_vol.id_aportante  CLIPPED,
                COLUMN 088,"       0.00",
                COLUMN 099,det_vol.m_pesos USING "#######&.&&"
        END IF
END REPORT
############################################################
----------------------------------------------Sumario Ahorro
REPORT SSUM_AHO()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER        #TOTAL AHORRO
        LET i_total_reg_lote = i_total_reg_lote + i_cont_de_reg
        PRINT
            COLUMN 01,"99" ,
            COLUMN 03,"13" ,
            COLUMN 05,i_cont_de_reg USING "##########&",
            COLUMN 16,vcargos_vol   USING "#######&.&&",
            COLUMN 27,vabonos_vol   USING "#######&.&&"
END REPORT
############################################################
-----------------------------------------------------Sumario
REPORT SSUM_DIS()

    OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 01,"98",
                COLUMN 03,"01",
                COLUMN 05,total_cargo_01 USING "#######&.&&",
                COLUMN 16,total_abono_01 USING "#######&.&&",
                COLUMN 27,total_final_01 USING "#######&.&&"
            PRINT
                COLUMN 01,"98",
                COLUMN 03,"02",
                COLUMN 05,total_cargo_02 USING "#######&.&&",
                COLUMN 16,total_abono_02 USING "#######&.&&",
                COLUMN 27,total_final_02 USING "#######&.&&"
            PRINT
                COLUMN 01,"98",
                COLUMN 03,"03",
                COLUMN 05,total_cargo_03 USING "#######&.&&",
                COLUMN 16,total_abono_03 USING "#######&.&&",
                COLUMN 27,total_final_03 USING "#######&.&&"

END REPORT
############################################################
-------------------------------Reporte encabezado trabajador
REPORT CZAINDD()

   OUTPUT
       PAGE LENGTH   2
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT
           COLUMN 001, "02",             #tipo registro
           COLUMN 003, s_folio  USING"&&&&&&&", #tipo registro
           COLUMN 010, vfecha_top     USING "DDMMYYYY",	 #fecha final
           COLUMN 018, w_aux.nombres[1,40] CLIPPED," ",
                       w_aux.paterno[1,40] CLIPPED," ",
                       w_aux.materno[1,40] CLIPPED,      #nombre completo
           COLUMN 138, w_aux.callep        CLIPPED," ",	 #calle y numero
                       w_aux.numep         CLIPPED," ",	 #colonia
                       w_aux.deptop        CLIPPED,
           COLUMN 200, w_aux.coloniap      CLIPPED,
           COLUMN 260, w_aux.codposp       CLIPPED,
           COLUMN 265, vcentro_reparto     CLIPPED,      #falta este
           COLUMN 270, estad               CLIPPED,
           COLUMN 310, delega              CLIPPED,
           COLUMN 350, w_aux.fentcons  USING "DDMMYYYY", #fecha certificacion
           COLUMN 358, w_aux.seguro,             #nss afiliado
           COLUMN 369, w_aux.rfc,                #rfc afiliado
           COLUMN 382, w_aux.n_unico,            #curp afiliado
           COLUMN 400, saldofra    USING "#######&.&&",  #saldo dia sar
           COLUMN 411, saldofsi    USING "#######&.&&",  #saldo dia sar issste
           COLUMN 422, saldofre    USING "#######&.&&",  #saldo dia rcv
           COLUMN 433, saldofcv    USING "#######&.&&",  #saldo dia cv
           COLUMN 444, saldofcs    USING "#######&.&&",  #saldo dia cs
           COLUMN 455, saldofvo    USING "#######&.&&",  #saldo dia vol
           COLUMN 466, saldofcr    USING "#######&.&&",  #saldo dia comp ret
           COLUMN 477, saldofah    USING "#######&.&&",  #saldo dia ahorro

           COLUMN 488, saldofvi    USING "#######&.&&",  #saldo dia viv97
           COLUMN 499, saldofva    USING "#######&.&&",  #saldo dia viv92
           COLUMN 510, saldofvs    USING "#######&.&&",  #saldo dia vivis

           COLUMN 521,saldof1re    USING "#######&.&&",	 #saldo siefore1
           COLUMN 532,saldof1cv	   USING "#######&.&&",
           COLUMN 543,saldof1cs	   USING "#######&.&&",
           COLUMN 554,saldof1ra    USING "#######&.&&",
           COLUMN 565,saldof1si	   USING "#######&.&&",

           COLUMN 576,saldof1vo	   USING "#######&.&&",
           COLUMN 587,saldof1cr	   USING "#######&.&&",
           COLUMN 598,saldof1ah	   USING "#######&.&&",

           COLUMN 609,saldof2vo	   USING "#######&.&&",	 #saldo siefore2
           COLUMN 620,saldof2re	   USING "#######&.&&",
           COLUMN 631,saldof2cv	   USING "#######&.&&",
           COLUMN 642,saldof2cs	   USING "#######&.&&",
           COLUMN 653,saldof2ra	   USING "#######&.&&",

           COLUMN 664,saldof2si	   USING "#######&.&&",
           COLUMN 675,saldof2cr	   USING "#######&.&&",
           COLUMN 686,saldof2ah	   USING "#######&.&&",

           COLUMN 697,saldof3re	   USING "#######&.&&",	 #saldo siefore3
           COLUMN 708,saldof3cv	   USING "#######&.&&",
           COLUMN 719,saldof3cs	   USING "#######&.&&",
           COLUMN 730,saldof3ra	   USING "#######&.&&",
           COLUMN 741,saldof3si	   USING "#######&.&&",

           COLUMN 752,saldof3vo	   USING "#######&.&&",
           COLUMN 763,saldof3cr	   USING "#######&.&&",
           COLUMN 774,saldof3ah	   USING "#######&.&&",

           COLUMN 785,tot_sie01_rcv USING "#######&.&&", #totales siefores
           COLUMN 797,tot_sie01_vol USING "#######&.&&",
           COLUMN 809,tot_sie02_rcv USING "#######&.&&",
           COLUMN 821,tot_sie02_vol USING "#######&.&&",
           COLUMN 833,tot_sie03_rcv USING "#######&.&&",
           COLUMN 845,tot_sie03_vol USING "#######&.&&",

           COLUMN 857, importe2    USING "########&.&&", #saldo dia vivienda
           COLUMN 869, importe1    USING "########&.&&", #suma de RCV
           COLUMN 881, importe4    USING "########&.&&", #suma de VOL
           COLUMN 893, importe3    USING "########&.&&", #suma rcv + viv + vol
           COLUMN 905, nom_sie1 ,
           COLUMN 909, nom_sie2 ,
           COLUMN 913, nom_sie3
END REPORT
############################################################
FUNCTION calcula_totales()

   LET total_abono_01 = x_abono_rcv +
                        x_abono_cv  +
                        x_abono_cs  +
                        x_abono_sar +
                        x_abono_saris

   LET total_cargo_01 = x_cargo_rcv +
                        x_cargo_cv  +
                        x_cargo_cs  +
                        x_cargo_sar +
                        x_cargo_saris

   LET total_abono_02 = x_abono_vol +
                        x_abono_aho +
                        x_abono_ret

   LET total_cargo_02 = x_cargo_vol +
                        x_cargo_aho +
                        x_cargo_ret

   LET total_abono_03 = x_abono_viv97 +
                        x_abono_viv92 +
                        x_abono_vivis

   LET total_cargo_03 = x_cargo_viv97 +
                        x_cargo_viv92 +
                        x_cargo_vivis

   LET total_final_01 = total_abono_01 + total_cargo_01
   LET total_final_02 = total_abono_02 + total_cargo_02
   LET total_final_03 = total_abono_03 + total_cargo_03

END FUNCTION
############################################################
FUNCTION calcula_dias(fecha)
#cd------------------------
    DEFINE   fecha   DATE

    CASE MONTH(fecha)            #Regresa dias del mes anterior
        WHEN  1   RETURN 31
        WHEN  2   IF YEAR(fecha) MOD 4 = 0 THEN
                      RETURN 29
                  ELSE
                      RETURN 28
                  END IF
        WHEN  3   RETURN 31
        WHEN  4   RETURN 30
        WHEN  5   RETURN 31
        WHEN  6   RETURN 30
        WHEN  7   RETURN 31
        WHEN  8   RETURN 31
        WHEN  9   RETURN 30
        WHEN 10   RETURN 31
        WHEN 11   RETURN 30
        WHEN 12   RETURN 31
    END CASE

END FUNCTION #calcula_dias
############################################################
FUNCTION habil_siguiente(diaActual)
#hs--------------------------------
   DEFINE diaTmp    DATE,
          contador  SMALLINT,
          diaActual DATE

   DEFINE diaHabilSig  DATE,
          diaSemana    SMALLINT,
          feriado      SMALLINT,
          finSemana    SMALLINT

   LET diaHabilSig = diaActual

  WHILE TRUE
     LET feriado   = 0
     LET finSemana = 0
     LET diaSemana = WEEKDAY(diaHabilSig)

     IF diaSemana = 0 OR diaSemana = 6 THEN
   	   LET finSemana = 1
     END IF

     SELECT *
     FROM   tab_feriado
     WHERE  feria_fecha = diaHabilSig

     IF STATUS <> NOTFOUND THEN
        LET feriado = 1
     END IF

     IF feriado = 1 OR finSemana = 1 THEN
        LET diaHabilSig = diaHabilSig + 1 UNITS DAY
     ELSE
        EXIT WHILE
     END IF
  END WHILE

  RETURN diaHabilSig

END FUNCTION #habil_siguiente
############################################################
FUNCTION habil_anterior(diaActual)
#ha--------------------------------

   DEFINE diaTmp            DATE,
           contador	    SMALLINT,
           diaActual	    DATE,
           diaHabilAnt      DATE,
           diaSemana	    SMALLINT,
           feriado          SMALLINT,
           finSemana	    SMALLINT

  LET diaHabilAnt = diaActual

  WHILE TRUE
     LET feriado   = 0
     LET finSemana = 0
     LET diaSemana = WEEKDAY(diaHabilAnt)

     IF diaSemana = 0 OR diaSemana = 6 THEN
        LET finSemana = 1
     END IF

     SELECT *
     FROM   tab_feriado
     WHERE  feria_fecha = diaHabilAnt

     IF STATUS <> NOTFOUND THEN
        LET feriado = 1
     END IF

     IF feriado = 1 OR finSemana = 1 THEN
        LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
     ELSE
        EXIT WHILE
     END IF
  END WHILE

  RETURN diaHabilAnt

END FUNCTION #habil_anterior
############################################################
FUNCTION mes_a_bim(anyo,mes)
#mab------------------------

   DEFINE
      anyo     CHAR(04),
      mes      CHAR(02),
      bimestre CHAR(06),
      mes_b    CHAR(01)

   IF anyo IS NOT NULL OR anyo<>" " THEN
      CASE mes
           WHEN "01" LET mes_b = "1"
           WHEN "02" LET mes_b = "1"
           WHEN "03" LET mes_b = "2"
           WHEN "04" LET mes_b = "2"
           WHEN "05" LET mes_b = "3"
           WHEN "06" LET mes_b = "3"
           WHEN "07" LET mes_b = "4"
           WHEN "08" LET mes_b = "4"
           WHEN "09" LET mes_b = "5"
           WHEN "10" LET mes_b = "5"
           WHEN "11" LET mes_b = "6"
           WHEN "12" LET mes_b = "6"
      END CASE

      LET bimestre = mes_b,"/",anyo
   ELSE
      LET bimestre = NULL
   END IF

   RETURN bimestre

END FUNCTION #mes_a_bimestre
############################################################
FUNCTION periodo_int(fecha_per)
#pi------------------------

   DEFINE
      fecha_per       DATE,
      anyomes         CHAR(10),
      mes_periodo     CHAR(07)

    LET anyomes       = fecha_per -1 UNITS MONTH
    LET mes_periodo   =  anyomes[06,07],"/",anyomes[01,04]
    LET vperiodo_int  = mes_periodo

    RETURN vperiodo_int
END FUNCTION #periodo_int
############################################################
FUNCTION actualiza_operacion()
#ao---------------------------

     UPDATE bat_ctr_operacion
     SET    fecha_fin        = CURRENT ,
            estado_operacion = 4
     WHERE  pid = reg_bat.pid
     AND    proceso_cod = reg_bat.proceso_cod
     AND    opera_cod   = reg_bat.opera_cod

END FUNCTION
############################################################
FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vestado,vresultado)

   DEFINE vfolio         INTEGER,
          vetapa_cod     DECIMAL(2,0),
          vresultado     CHAR(80),
          vestado        SMALLINT

   DEFINE hora_inicial       CHAR(08),
          hora_final         CHAR(08)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES
      (TODAY,             -- fecha_proceso
       "CTA",             -- proceso_cod
       vetapa_cod,        -- etapa_cod
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       NULL,              -- parametro1
       vestado,           -- parametro2
       NULL,              -- parametro3
       NULL,              -- parametro4
       NULL,              -- parametro5
       vfolio,            -- folio
       vresultado,        -- resultado
       USER,              -- usuario
       0                  -- consecutivo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",
            vetapa_cod," ",STATUS
      EXIT PROGRAM
   END IF
END FUNCTION
############################################################
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vpos,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(80),
          vpos         INTEGER,
          hoy          DATE

   DEFINE hora_inicial       CHAR(08),
          vhora_final        CHAR(08)

   DEFINE cla_sel            CHAR(400),
          vconsecutivo       INTEGER

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  folio  = ",vfolio,
                 " AND    proceso_cod = 'CTA' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe3 FROM cla_sel
   DECLARE cur_proceso3 CURSOR FOR claexe3

   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vconsecutivo
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        parametro3 = ",vpos,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod   = 'CTA'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED

   PREPARE claexe4 FROM cla_sel
   EXECUTE claexe4

END FUNCTION
############################################################
#eof
