############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TAAB025  => CONSULTA SDO TRASPASO DE AFORE RECEPTORA ( Previo  ) #
#Fecha             => 20 DE FEBRERO DE 2009                                #
#Autor             => FERNANDO HERRERA HERNANDEZ                           #
#Sistema           => TAA ( Operacion 29 )                                 #
# Adecuacion circular 28-20 Liquidacion Bimestral                          #
############################################################################

DATABASE safre_af

GLOBALS

  DEFINE reg ARRAY[30]       OF RECORD
    folio                    DECIMAL(8,0),
    siefore                  CHAR(03),
    saldo_rcv                DECIMAL(15,2),
    num_acciones_rcv         DECIMAL(22,6)
  END RECORD

  DEFINE reg_mae             RECORD
    folio                    DECIMAL(8,0),
    nss                      CHAR(11),
    siefore                  CHAR(03),
    cve_subcta               CHAR(02),
    saldo_rcv                DECIMAL(15,2),
    num_acciones_rcv         DECIMAL(22,6),
    fena                     DATE 
  END RECORD

  DEFINE reg2 ARRAY[100]     OF RECORD
    folio                    DECIMAL(8,0),
    siefore                  CHAR(03),
    cve_subcta               CHAR(02),
    saldo_rcv                DECIMAL(15,2),
    num_acciones_rcv         DECIMAL(22,6)
  END RECORD
    
  DEFINE 
    nss                      CHAR(11),
    fecha_valor              DATE    ,
    folio                    INTEGER 

  DEFINE
    g_afore                  RECORD LIKE tab_afore_local.*,
    g_paramgrales            RECORD LIKE seg_modulo.*

  DEFINE
    HOY                      DATE

  DEFINE
    enter                    CHAR(1),
    HORA                     CHAR(8),
    g_usuario                CHAR(8) 

  DEFINE
    i                        SMALLINT,
    pos                      INTEGER,
    sel_where                CHAR(1000),
    cla_where                CHAR(1000),
    sel_where_rcv            CHAR(1000),
    cla_where_rcv            CHAR(1000),
    sel_where_viv            CHAR(1000),
    cla_where_viv            CHAR(1000),
    sel_where_rcvi           CHAR(1000),
    cla_where_rcvi           CHAR(1000),
    sel_where_bono           CHAR(1000),
    cla_where_bono           CHAR(1000),
    sel_where_vol            CHAR(1000),
    cla_where_vol            CHAR(1000),
    sel_where_mae            CHAR(1000),
    cla_where_mae            CHAR(1000),
    total_saldo_rcv          DECIMAL(15,2),
    total_acciones_rcv       DECIMAL(22,6),
    total_saldo_viv          DECIMAL(15,2),
    total_acciones_viv       DECIMAL(22,6),
    total_saldo_rcvi         DECIMAL(15,2),
    total_acciones_rcvi      DECIMAL(22,6),
    total_saldo_bono         DECIMAL(15,2),
    total_acciones_bono      DECIMAL(22,6),
    total_saldo_vol          DECIMAL(15,2),
    total_acciones_vol       DECIMAL(22,6)
   DEFINE
      vfecha_mes               CHAR(2),
      vfecha_dia               CHAR(2),
      vfecha_anio              CHAR(4),
      vfecha_comp              CHAR(10),
      v_sql_1                  CHAR(50)

   DEFINE
      v_edad                   SMALLINT,
      v_criterio               SMALLINT,
      v_crea_fecha             DATE
    

END GLOBALS

MAIN

  DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP

    CALL inicio()
    CALL STARTLOG("TAAB025.log")
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

  LET HOY   = TODAY
  LET HORA  = TIME

  SELECT *, USER
  INTO   g_paramgrales.*, g_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'taa'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

  OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TAAB0251" ATTRIBUTE( BORDER)

  DISPLAY " TAAB025         CONSULTA SDOS TAA  DE AFORE RECEPTORA PREVIO                  " AT 3,1 ATTRIBUTE(REVERSE)

  DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

  MENU " TRASPASOS PREVIOS RECIBIDOS "
    COMMAND KEY (A) "sAldos Procesar" " Consulta Saldos Traspasos Procesar"
      CALL Consulta()
    COMMAND KEY(L) "saLdos por Edad" " Consulta Saldos Traspasos asignando siefore"
      CALL Consulta_edad()
      CALL limpia()
    COMMAND KEY(D) "salDos por Subcuenta" " Consulta Saldos Traspasos por Subcuenta"
      CALL Consulta_subcuenta()
    COMMAND KEY(S) "Salir " " Salir de Programa"
      EXIT MENU
  END MENU

END FUNCTION

FUNCTION Inicializa()
#iz------------------

  DEFINE j SMALLINT

  INITIALIZE reg  TO NULL
  INITIALIZE reg2 TO NULL

  FOR j = 1 TO 10
      DISPLAY reg[i].*  TO scr_1[i].* ATTRIBUTE (NORMAL)
  END FOR

  FOR j = 1 TO 10
      DISPLAY reg2[i].* TO scr_2[i].* ATTRIBUTE (NORMAL)
  END FOR

  CLEAR FORM

END FUNCTION

FUNCTION Consulta_subcuenta()
#Cs-----------------

  OPEN WINDOW ventana_2 AT 3,2 WITH FORM "TAAB0252" ATTRIBUTE( BORDER)

  DISPLAY " TAAB025         CONSULTA SDOS TAA  DE AFORE RECEPTORA PREVIO                  " AT 3,1 ATTRIBUTE(REVERSE)

  DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

  DISPLAY " FOLIO    SIEFORE  SUBCUENTA  SALDO PESOS            SALDO ACCIONES            " AT 5,1 ATTRIBUTE(REVERSE)

  LET int_flag = FALSE

  CONSTRUCT cla_where ON folio
                    FROM folio

  IF int_flag = TRUE THEN
     LET int_flag = FALSE
     ERROR "BUSQUEDA CANCELADA..."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     RETURN
  END IF

  LET sel_where = " SELECT a.folio,",
                         " a.siefore,",
                         " a.cve_subcta,",
                         " sum(saldo),",
                         " sum(num_acciones)",
                  " FROM det_tra_sdo_previo a",
                  " WHERE ", cla_where CLIPPED,
                  " AND cve_subcta <> ''",
                  " GROUP BY 1,2,3",
                  " ORDER BY 1,2,3"

  LET sel_where = sel_where CLIPPED

  PREPARE qry_consul_s FROM sel_where 
  DECLARE cursor_s CURSOR FOR qry_consul_s

  LET pos                = 1

  FOREACH cursor_s INTO reg2[pos].folio THRU reg2[pos].num_acciones_rcv

    LET pos = pos + 1

  END FOREACH

  ----- SALDOS RCV -----
  LET sel_where_rcv = " SELECT sum(saldo),",
                      " sum(num_acciones)",
                      " FROM   det_tra_sdo_previo a ",
                      " WHERE ", cla_where CLIPPED,
                      " AND cve_subcta in ('01','02','03','08') "

  LET sel_where_rcv = sel_where_rcv CLIPPED

  PREPARE qry_consul_rcv FROM sel_where_rcv 
  DECLARE cursor_c_rcv CURSOR FOR qry_consul_rcv

  LET total_saldo_rcv    = 0
  LET total_acciones_rcv = 0

  FOREACH cursor_c_rcv INTO total_saldo_rcv, total_acciones_rcv           
  END FOREACH

  DISPLAY total_saldo_rcv    TO total_saldo_rcv
  DISPLAY total_acciones_rcv TO total_acciones_rcv
  ----- SALDOS RCV -----


  ----- SALDOS VIV -----
  LET sel_where_viv = " SELECT sum(saldo),",
                      " sum(num_acciones)",
                      " FROM   det_tra_sdo_previo a ",
                      " WHERE ", cla_where CLIPPED,
                      " AND cve_subcta in ('04','09') "

  LET sel_where_viv = sel_where_viv CLIPPED

  PREPARE qry_consul_viv_s FROM sel_where_viv 
  DECLARE cursor_s_viv CURSOR FOR qry_consul_viv_s

  LET total_saldo_viv    = 0
  LET total_acciones_viv = 0

  FOREACH cursor_s_viv INTO total_saldo_viv, total_acciones_viv           
  END FOREACH
  
  DISPLAY total_saldo_viv    TO total_saldo_viv
  DISPLAY total_acciones_viv TO total_acciones_viv
  ----- SALDOS VIV -----


  ----- SALDOS VOL -----
  LET sel_where_vol = " SELECT sum(saldo),",
                      " sum(num_acciones)",
                      " FROM   det_tra_sdo_previo a ",
                      " WHERE ", cla_where CLIPPED,
                      " AND cve_subcta in ('05','13','15','17',",
                                          "'18','19') "

  LET sel_where_vol = sel_where_vol CLIPPED

  PREPARE qry_consul_vol FROM sel_where_vol 
  DECLARE cursor_c_vol CURSOR FOR qry_consul_vol

  LET total_saldo_vol    = 0
  LET total_acciones_vol = 0

  FOREACH cursor_c_vol INTO total_saldo_vol, total_acciones_vol           
  END FOREACH
  
  DISPLAY total_saldo_vol    TO total_saldo_vol
  DISPLAY total_acciones_vol TO total_acciones_vol
  ----- SALDOS VOL -----


  ----- SALDOS RCV ISSSTE -----
  LET sel_where_rcvi = " SELECT sum(saldo),",
                      " sum(num_acciones)",
                      " FROM   det_tra_sdo_previo a ",
                      " WHERE ", cla_where CLIPPED,
                      " AND cve_subcta in ('20','21','22','23','24',",
                                          "'25','29') "

  LET sel_where_rcvi = sel_where_rcvi CLIPPED

  PREPARE qry_consul_rcvi FROM sel_where_rcvi 
  DECLARE cursor_c_rcvi CURSOR FOR qry_consul_rcvi

  LET total_saldo_rcvi    = 0
  LET total_acciones_rcvi = 0

  FOREACH cursor_c_rcvi INTO total_saldo_rcvi, total_acciones_rcvi
  END FOREACH

  DISPLAY total_saldo_rcvi    TO total_saldo_rcvi
  DISPLAY total_acciones_rcvi TO total_acciones_rcvi
  ----- SALDOS RCV ISSSTE -----


  ----- SALDOS BONO ISSSTE -----
  LET sel_where_bono = " SELECT sum(saldo),",
                      " sum(num_acciones)",
                      " FROM   det_tra_sdo_previo a ",
                      " WHERE ", cla_where CLIPPED,
                      " AND cve_subcta in ('27') "

  LET sel_where_bono = sel_where_bono CLIPPED

  PREPARE qry_consul_bono FROM sel_where_bono 
  DECLARE cursor_c_bono CURSOR FOR qry_consul_bono

  LET total_saldo_bono    = 0
  LET total_acciones_bono = 0

  FOREACH cursor_c_bono INTO total_saldo_bono, total_acciones_bono
  END FOREACH

  DISPLAY total_saldo_bono    TO total_saldo_bono
  DISPLAY total_acciones_bono TO total_acciones_bono
  ----- SALDOS BONO ISSSTE -----

  IF (pos-1) >= 1 THEN
     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY reg2 TO scr_2.*

     IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA TERMINADA..."
        SLEEP 2
        ERROR ""
        CLOSE WINDOW ventana_2
        RETURN
     END IF
  ELSE
     ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
     SLEEP 2
     ERROR ""
  END IF

  CLOSE WINDOW ventana_2
  RETURN

END FUNCTION

FUNCTION Consulta_edad()
#Ce-----------------

  DISPLAY " FOLIO    SIEFORE   SALDO PESOS                      SALDO ACCIONES            " AT 5,1 ATTRIBUTE(REVERSE)

  LET int_flag = FALSE

  CONSTRUCT cla_where ON folio
                    FROM folio

  IF int_flag = TRUE THEN
     LET int_flag = FALSE
     ERROR "BUSQUEDA CANCELADA..."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     RETURN
  END IF
  
  CALL crea_tabla()

  LET sel_where_mae = " SELECT a.folio,",
                      " a.nss,",
                      " a.siefore,",
                      " a.cve_subcta,",
                      " a.saldo,",
                      " a.num_acciones, ",
                      " ' '",
                      " FROM   det_tra_sdo_previo a",
                      " WHERE ", cla_where CLIPPED

  LET sel_where_mae = sel_where_mae CLIPPED

  PREPARE qry_consul_m FROM sel_where_mae 
  DECLARE cursor_mae CURSOR FOR qry_consul_m

  FOREACH cursor_mae INTO reg_mae.folio THRU reg_mae.fena

    DECLARE cur_edad CURSOR FOR
    SELECT fena
    FROM   afi_solicitud
    WHERE  n_seguro = reg_mae.nss 
    #AND    status_interno IN  (50,70)
    FOREACH cur_edad INTO reg_mae.fena
      CONTINUE FOREACH
    END FOREACH 

    CALL calcula_edad_sie()
    
    LET reg_mae.siefore = "SB" CLIPPED, v_criterio CLIPPED
    
    INSERT INTO safre_tmp:tmp_sdos_previo VALUES (reg_mae.folio,
                                        reg_mae.siefore,
                                        reg_mae.cve_subcta,
                                        reg_mae.saldo_rcv,
                                        reg_mae.num_acciones_rcv)
   
  END FOREACH   

  DECLARE cur_sie1 CURSOR FOR
  SELECT a.folio,
         a.siefore,
         sum(saldo),
         sum(num_acciones)
  FROM   safre_tmp:tmp_sdos_previo a
  WHERE  cve_subcta not in ('04','09')
  GROUP BY 1,2
  ORDER BY 2
 
  LET pos                = 1
  LET total_saldo_rcv    = 0
  LET total_acciones_rcv = 0

  FOREACH cur_sie1 INTO reg[pos].folio THRU reg[pos].num_acciones_rcv

    LET total_saldo_rcv    = total_saldo_rcv    + reg[pos].saldo_rcv
    LET total_acciones_rcv = total_acciones_rcv + reg[pos].num_acciones_rcv

    LET pos = pos + 1

  END FOREACH

  DISPLAY total_saldo_rcv    TO total_saldo_rcv
  DISPLAY total_acciones_rcv TO total_acciones_rcv

  DECLARE cur_sie2 CURSOR FOR
  SELECT sum(b.saldo),
         sum(b.num_acciones)
  FROM   safre_tmp:tmp_sdos_previo b
  WHERE  b.cve_subcta IN ('04','09')

  LET total_saldo_viv    = 0
  LET total_acciones_viv = 0

  FOREACH cur_sie2 INTO total_saldo_viv, total_acciones_viv
  END FOREACH

  DISPLAY total_saldo_viv    TO total_saldo_viv
  DISPLAY total_acciones_viv TO total_acciones_viv

  IF (pos-1) >= 1 THEN
     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY reg TO scr_1.*

     IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA TERMINADA..."
        SLEEP 2
        ERROR ""
        CLEAR FORM
        RETURN
     END IF
  ELSE
     ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
     SLEEP 2
     ERROR ""
  END IF

END FUNCTION

FUNCTION crea_tabla()

  WHENEVER ERROR CONTINUE
    DROP TABLE safre_tmp:tmp_sdos_previo;
    database safre_tmp
    CREATE TABLE tmp_sdos_previo (folio                    DECIMAL(8,0),
                                  siefore                  CHAR(03),
                                  cve_subcta               CHAR(02),
                                  saldo                    DECIMAL(15,2),
                                  num_acciones             DECIMAL(22,6))
  WHENEVER ERROR STOP
    
    database safre_af
  LET v_sql_1 = "EXECUTE PROCEDURE fn_edad_multisie(?,?)"
  PREPARE stmt1 FROM v_sql_1

END FUNCTION

FUNCTION calcula_edad_sie()

  LET v_crea_fecha = HOY

  DECLARE curs1 CURSOR FOR stmt1
  OPEN  curs1 USING reg_mae.fena, v_crea_fecha
  FETCH curs1 INTO v_edad, v_criterio
  CLOSE curs1

  SELECT ind_edad
  INTO   v_criterio
  FROM tab_rango_edad
  WHERE v_edad BETWEEN edad_min AND edad_max;

  #LET v_criterio = v_criterio using "##"

END FUNCTION

FUNCTION Consulta()
#C-----------------

  DISPLAY " FOLIO    SIEFORE   SALDO PESOS                      SALDO ACCIONES            " AT 5,1 ATTRIBUTE(REVERSE)

  LET int_flag = FALSE

  CONSTRUCT cla_where ON folio
                    FROM folio

  IF int_flag = TRUE THEN
     LET int_flag = FALSE
     ERROR "BUSQUEDA CANCELADA..."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     RETURN
  END IF

  LET sel_where = " SELECT a.folio,",
                         " a.siefore,",
                         " sum(saldo),",
                         " sum(num_acciones)",
                  " FROM det_tra_sdo_previo a",
                  " WHERE ", cla_where CLIPPED,
                  " AND cve_subcta not in ('04','09') ",
                  " GROUP BY 1,2",
                  " ORDER BY 2"

  LET sel_where = sel_where CLIPPED

  PREPARE qry_consul FROM sel_where 
  DECLARE cursor_c CURSOR FOR qry_consul

  LET pos                = 1
  LET total_saldo_rcv    = 0
  LET total_acciones_rcv = 0

  FOREACH cursor_c INTO reg[pos].folio THRU reg[pos].num_acciones_rcv

    LET total_saldo_rcv    = total_saldo_rcv    + reg[pos].saldo_rcv
    LET total_acciones_rcv = total_acciones_rcv + reg[pos].num_acciones_rcv

    LET pos = pos + 1

  END FOREACH
  
  DISPLAY total_saldo_rcv    TO total_saldo_rcv
  DISPLAY total_acciones_rcv TO total_acciones_rcv

  LET sel_where_viv = " SELECT sum(saldo),",
                      " sum(num_acciones)",
                      " FROM   det_tra_sdo_previo a ",
                      " WHERE ", cla_where CLIPPED,
                      " AND cve_subcta in ('04','09') "

  LET sel_where_viv = sel_where_viv CLIPPED

  PREPARE qry_consul_viv FROM sel_where_viv 
  DECLARE cursor_c_viv CURSOR FOR qry_consul_viv

  LET total_saldo_viv    = 0
  LET total_acciones_viv = 0

  FOREACH cursor_c_viv INTO total_saldo_viv, total_acciones_viv           
  END FOREACH
  
  DISPLAY total_saldo_viv    TO total_saldo_viv
  DISPLAY total_acciones_viv TO total_acciones_viv

  IF (pos-1) >= 1 THEN
     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY reg TO scr_1.*

     IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA TERMINADA..."
        SLEEP 2
        ERROR ""
        CLEAR FORM
        RETURN
     END IF
  ELSE
     ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
     SLEEP 2
     ERROR ""
  END IF

END FUNCTION

FUNCTION limpia()

 WHENEVER ERROR CONTINUE
  database safre_tmp
  DROP TABLE tmp_sdos_previo;
 WHENEVER ERROR STOP

  database safre_af
  INITIALIZE reg        TO NULL
  INITIALIZE reg_mae    TO NULL  
  LET pos                = 0
  LET total_saldo_viv    = 0
  LET total_acciones_viv = 0
  LET total_saldo_rcv    = 0
  LET total_acciones_rcv = 0

  DISPLAY total_saldo_rcv    TO total_saldo_rcv
  DISPLAY total_acciones_rcv TO total_acciones_rcv
  DISPLAY total_saldo_viv    TO total_saldo_viv
  DISPLAY total_acciones_viv TO total_acciones_viv

END FUNCTION
