#########################################################################
#Proyecto          => AFORE ( MEXICO )                                  
#Propietario       => E.F.P.                                            
#Modulo            => SEP (Separacion de Cuentas)
#Programa SEPL030  => CONSULTA DETALLE DE MOVIMIENTOS INCONSISTENTES 
#Fecha             => 3 abr 2010
#Autor             => JESUS DAVID YANEZ MORENO                          
#########################################################################

DATABASE safre_af

GLOBALS

    DEFINE  i               ,
            tot             ,
            lastkey         ,
            arr_c           ,
            arr_c_i         ,
            total_pa        ,
            pos             ,
            linea_i         ,
            linea           SMALLINT

    DEFINE reg_row ARRAY[20000] OF RECORD 
        row                 INTEGER
    END RECORD 

    DEFINE reg ARRAY[20000] OF RECORD
        indice             CHAR(01)   ,
        consec             INTEGER    ,
        tipo_movimiento    SMALLINT   ,
        subcuenta          SMALLINT   ,
        folio              INTEGER    ,
        fecha_valor        DATE       ,
        monto_en_acciones  DEC(13,6)  ,
        monto_en_pesos     DEC(13,6)  ,
        id_aportante       CHAR(011)  ,
        ind_mov_asociado   SMALLINT
    END RECORD

    DEFINE reg_id ARRAY[20000] OF RECORD
           idSepMovimientosInvadido   ,
           siefore                    integer
    END RECORD

    DEFINE reg_resto ARRAY[20000] OF RECORD 
        consecutivo_lote   INTEGER  ,
        fecha_pago         DATE     ,
        fecha_proceso      DATE     ,
        folio_sua          CHAR(010),
        fecha_conversion   DATE     ,
        precio_accion      DEC(16,6)
    END RECORD

    DEFINE g_afore       RECORD LIKE tab_afore_local.*,
           g_paramgrales RECORD LIKE seg_modulo.*

    DEFINE HOY                          DATE

    DEFINE  g_folio_sep                 , 
            consec                      INTEGER

    DEFINE  g_asociado                   CHAR(011),
            g_invadido                   CHAR(011),
            g_diagnostico                CHAR(002),
            g_clasificacion              CHAR(001),
            g_enter                      CHAR(1)  ,
            g_usuario                    CHAR(8)  ,
            sel_where                    CHAR(2000)
END GLOBALS

MAIN

    DEFINE salida SMALLINT

    OPTIONS
    PROMPT LINE LAST, 
    INPUT WRAP
    DEFER INTERRUPT

    CALL STARTLOG("SEPL010.log")

    LET g_folio_sep             = ARG_VAL(1)
    LET g_invadido              = ARG_VAL(2)
    LET g_asociado              = ARG_VAL(3)
    LET g_diagnostico           = ARG_VAL(4)
    LET g_clasificacion         = ARG_VAL(5)

    CALL inicio()

    LET salida = 1

    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------
    DEFINE  l_qry               CHAR(1000)

    LET HOY   = TODAY

    SELECT  *, USER
      INTO  g_paramgrales.*, g_usuario
      FROM  seg_modulo
     WHERE  modulo_cod = 'taa';

     LET      l_qry           =
              " SELECT  c.n_rfc, ",
              "         TRIM(c.nombres)||' '||TRIM(c.paterno)||' '|| ",
              "         TRIM(c.materno),                             ",
              "         c.finicta                                    ",
              " FROM  afi_mae_afiliado c                             ",
              " WHERE c.n_seguro  =  ?                               "

     

     PREPARE   qry_1           FROM  l_qry
                       
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "SEPL010" ATTRIBUTE( BORDER)

    DISPLAY " <Ctr-c>Salir                                                                                   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " SEPL010        DETALLE DE MOVIMIENTOS DETECTADOS CON INONSISTENCIA                             " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
        CLEAR FORM
        CALL Consulta()
        CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION Inicializa()
#iz------------------

    DEFINE j SMALLINT

    INITIALIZE reg TO NULL
    INITIALIZE reg_resto TO NULL

    FOR j = 1 TO 12
        DISPLAY reg[i].* TO scr_1[i].* ATTRIBUTE (NORMAL)
    END FOR

    CLEAR FORM

END FUNCTION

FUNCTION Consulta()
#C-----------------

    DEFINE sali      SMALLINT
    DEFINE linea     SMALLINT

    LET int_flag = FALSE
    LET sali     = 0

    LET sel_where = " SELECT b.rowid            , ",
                    "        b.tipo_movimiento  , ",
                    "        b.subcuenta        , ",
                    "        b.folio            , ",
                    "        b.fecha_valor      , ",
                    "        b.monto_en_acciones, ",
                    "        b.monto_en_pesos   , ",
                    "        b.id_aportante     , ",
                    "        b.ind_mov_asociado , ",
                    "        b.consecutivo_lote , ",
                    "        b.fecha_pago       , ", 
                    "        b.fecha_proceso    , ",
                    "        b.folio_sua        , ",
                    "        b.fecha_conversion , ",
                    "        b.precio_accion    , ",
                    "        b.idSepMovimientosInvadido, ",
                    "        b.siefore ",
                    " FROM   sep_movimientos_invadido_v b ",
                    " WHERE  b.folio = ",g_folio_sep ,
                    " and    b.nss  = '",g_invadido,"'",  
                    " AND    b.tipo_movimiento not in (210,215,15) ",
                    " AND    b.id_aportante[1,3] not in (SELECT ",
                    "      c.id_aportante[1,3] FROM tes_tipo_id_aportante c) ",
                    " AND  b.ind_mov_asociado <> 0 ",
                    " ORDER BY 3,14,2 " 
    LET sel_where = sel_where CLIPPED
    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul
    LET pos = 1
    FOREACH cursor_c INTO reg_row[pos].row           ,
                          reg[pos].tipo_movimiento   ,
                          reg[pos].subcuenta         ,
                          reg[pos].folio             ,
                          reg[pos].fecha_valor       ,
                          reg[pos].monto_en_acciones ,
                          reg[pos].monto_en_pesos    ,
                          reg[pos].id_aportante      ,
                          reg[pos].ind_mov_asociado  ,
                          reg_resto[pos].*           ,
                          reg_id[pos].idSepMovimientosInvadido,
                          reg_id[pos].siefore
             LET reg[pos].consec = pos 
             LET pos             = pos + 1 

    END FOREACH

    LET tot = pos -1

    DISPLAY tot         TO FORMONLY.t_reg
    DISPLAY g_invadido  TO FORMONLY.g_invadido
    DISPLAY g_asociado  TO FORMONLY.g_asociado
   
    CALL SET_COUNT(pos-1)
    IF    (pos - 1)                     >=  1    THEN
           INPUT   ARRAY  reg  WITHOUT DEFAULTS  FROM  scr_1.*
           BEFORE  ROW
                LET arr_c    =  ARR_CURR()
                LET total_pa =  ARR_COUNT()
                LET linea    =  SCR_LINE()

                IF arr_c > total_pa THEN
                   EXIT INPUT
                END IF
           DISPLAY reg[arr_c].* TO scr_1[linea].* ATTRIBUTE(REVERSE) 
           CALL  datos_complemento(arr_c)

          AFTER FIELD indice

                 IF reg[arr_c].indice IS NOT NULL THEN
                    LET reg[arr_c].indice = NULL
                    DISPLAY  BY NAME reg[arr_c].indice
                 END IF

                 IF arr_c >= (total_pa) THEN
                    LET  lastkey = FGL_LASTKEY()
                      IF  ((lastkey = FGL_KEYVAL("down"))
                       OR  (lastkey = FGL_KEYVAL("return"))
                       OR  (lastkey = FGL_KEYVAL("tab"))
                       OR  (lastkey = FGL_KEYVAL("right")))
                      THEN
                        ERROR "No hay mas opciones en esa direccion."
                        NEXT FIELD indice
                      END IF
                 END IF

           AFTER ROW
              LET arr_c =  ARR_CURR()
              LET linea =  SCR_LINE()

              DISPLAY reg[arr_c].* TO scr_1[linea].*

      END INPUT
    ELSE
        PROMPT " NO EXISTEN REGISTROS .... <enter> Salir" FOR g_enter
    END IF

END FUNCTION
################################################################################
FUNCTION datos_complemento(l_arr_c)

    DEFINE   l_arr_c                  INTEGER
    DEFINE   l_nombre                 CHAR(50)
    DEFINE   l_rfc                    CHAR(13)
    DEFINE   l_finicta                DATE
   

    EXECUTE  qry_1 USING g_invadido INTO  l_rfc,l_nombre, l_finicta

    DISPLAY  l_nombre               TO  FORMONLY.nombre
    DISPLAY  l_rfc                  TO  FORMONLY.rfc
    DISPLAY  l_finicta              TO  FORMONLY.fec_afil
    DISPLAY  g_diagnostico          TO  FORMONLY.g_diagnostico
    DISPLAY  g_clasificacion        TO  FORMONLY.g_clasificacion

    DISPLAY BY NAME reg_resto[l_arr_c].consecutivo_lote
    DISPLAY BY NAME reg_resto[l_arr_c].fecha_pago
    DISPLAY BY NAME reg_resto[l_arr_c].fecha_proceso
    DISPLAY BY NAME reg_resto[l_arr_c].folio_sua
    DISPLAY BY NAME reg_resto[l_arr_c].fecha_conversion
    DISPLAY BY NAME reg_resto[l_arr_c].precio_accion

END FUNCTION 

