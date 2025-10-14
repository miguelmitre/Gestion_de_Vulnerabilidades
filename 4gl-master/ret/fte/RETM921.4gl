################################################################################
##Proyecto           => SYSTEMA DE AFORES ( MEXICO )                          ##
##Owner              => E.F.P.                                                ##
##Programa RETI100   => Administrar Solicitudes de Retiro - Globales          ##
##Sistema            => SAFRE                                                 ##
##Fecha Creacion     => 1 Oct 2009                                            ##
##By                 => JGHM                                                  ##
##Fecha Modifica     =>                                                       ##
################################################################################
#Fecha Actualiz.   => 22/01/22015                                               #
#Actualizacion     =>Cesar D. Chavéz  Mtz.                                      #
#                  => Se agrega solicitud de constancia en las pantallas de     #
#                     Agregar, Consulta, Modificacion y Elimina para los retiros#
#                     Totales y parciales CPL-1844                              #
#################################################################################

DATABASE safre_af

GLOBALS

  DEFINE     gc_opc                    CHAR(01)
  DEFINE     gc_curp                   CHAR(18)
  DEFINE     gc_nti                    CHAR(11)
  DEFINE     gc_tipo_retiro            CHAR(02)
  DEFINE     gi_consecutivo            INTEGER
  DEFINE     gd_today                  DATE
  DEFINE     gs_afores                 CHAR(03)
  DEFINE     gc_usuario                CHAR(20)
  DEFINE     gc_query                  CHAR(3000)
  DEFINE     gc_query1                 CHAR(3000)
  DEFINE     gs_Grabe                  SMALLINT
  DEFINE     gs_Cance                  SMALLINT
  DEFINE     gs_Muerto                 SMALLINT
  DEFINE     gs_disp_issste            SMALLINT
  DEFINE     gs_par_issste             SMALLINT

  DEFINE     gr_afi_mae_afiliado       RECORD LIKE  safre_af:afi_mae_afiliado.*
  DEFINE     gr_ret_sol_issste_tx      RECORD LIKE  safre_af:ret_sol_issste_tx.*
  DEFINE     gr_ret_solicitante        RECORD LIKE  safre_af:ret_solicitante.*
  DEFINE     gr_DatMar                 RECORD LIKE  safre_af:ret_datamart_issste.*
  DEFINE     gr_DatMarPar              RECORD LIKE  safre_af:ret_datamart_par_issste.*
  DEFINE     gr_MatDer                 RECORD LIKE  safre_af:ret_matriz_derecho_issste.*
  DEFINE     gc_num_cuenta                    LIKE  ret_beneficiario.num_cuenta
  DEFINE     gi_folio                         LIKE  ret_sol_issste_tx.folio_solicitud
  DEFINE     gr_ret_parcial_issste     RECORD LIKE  safre_af:ret_parcial_issste.*
  DEFINE     gr_ret_monto_par_issste   RECORD LIKE  safre_af:ret_monto_par_issste.*
  DEFINE     gi_siefore                       LIKE  safre_af:dis_cuenta.siefore

  #CPL-1844
  DEFINE     gr_constancia RECORD
  	 constancia CHAR(01)
  END RECORD
END GLOBALS


### Despliega menaje de error en pantalla, espera y limpia
FUNCTION f_DespMen(lc_Mens)
  DEFINE      lc_Mens                  CHAR(200)

  ERROR  lc_Mens
  SLEEP 3
  ERROR  ""
END FUNCTION


### Despliega mensaje pprompt, enter para continuar
FUNCTION f_DespPro(lc_Mens1, lc_Mens2, lc_Mens3)
  DEFINE      lc_Mens1                 CHAR(200)
  DEFINE      lc_Mens2                 CHAR(200)
  DEFINE      lc_Mens3                 CHAR(200)
  DEFINE      enter                    CHAR(01)

  PROMPT " ",lc_Mens1 CLIPPED, " ",lc_Mens2 CLIPPED,"  ",lc_Mens3 CLIPPED, " <ENTER> CONTINUAR" FOR CHAR enter
END FUNCTION


### Marcaje
FUNCTION f_Marcaje(lc_nss, li_consecutivo, ls_tipo_retiro)
  DEFINE      lc_nss                   LIKE  afi_mae_afiliado.n_seguro
  DEFINE      li_consecutivo           LIKE  ret_sol_issste_tx.consecutivo
  DEFINE      ls_tipo_retiro           LIKE  tab_ret_issste.tipo_retiro
  DEFINE      lc_movimiento            LIKE  tab_ret_issste.movimiento
  DEFINE      ls_marca_res             SMALLINT
  DEFINE      ls_rechazo_cod           SMALLINT
  DEFINE      lc_desc_rechazo          CHAR(100)
  DEFINE      ls_Resul                 SMALLINT

  SELECT  movimiento
    INTO  lc_movimiento
    FROM  tab_ret_issste
   WHERE  tipo_retiro   = ls_tipo_retiro
   AND    cod_tramite   IN (gs_disp_issste, gs_par_issste)

  CALL marca_cuenta (lc_nss,
                     lc_movimiento,
                     li_consecutivo)
          RETURNING  ls_marca_res ,
                     ls_rechazo_cod

  IF   ls_rechazo_cod     >      0 THEN
       SELECT  rechazo_desc
         INTO  lc_desc_rechazo
         FROM  tab_rch_marca
        WHERE  rechazo_cod = ls_rechazo_cod

       CALL f_DespPro(" RECHAZADA ", ls_rechazo_cod CLIPPED, lc_desc_rechazo CLIPPED)
       LET     ls_Resul     =   1
  ELSE
       LET     ls_Resul     =   0
  END IF
  RETURN  ls_Resul
END FUNCTION


### Detalle del marcaje
FUNCTION marca_cuenta(vl_nss,vl_marca_ent,vl_consecutivo)
#mc------------------------------------------------------
  DEFINE #loc #smallint
       vl_marca_ent        ,
       vl_marca_res        ,
       vl_rechazo_cod                     SMALLINT

  DEFINE #loc #char
       vl_nss                             CHAR(011)

  DEFINE #loc #integer
       vl_consecutivo                     INTEGER
  DEFINE   v_marca                        CHAR(0100)
  DEFINE reg_20 RECORD #glo #reg_20
         estado_marca                     SMALLINT ,
         codigo_rechazo                   SMALLINT ,
         marca_causa                      SMALLINT ,
         fecha_causa                      DATE
         END RECORD

  LET        v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"
  LET        reg_20.estado_marca        =  0
  LET        reg_20.codigo_rechazo      =  0
  LET        reg_20.marca_causa         =  0
  INITIALIZE reg_20.fecha_causa         TO NULL

  PREPARE eje_marca FROM v_marca
  DECLARE cur_sp CURSOR FOR eje_marca

  OPEN    cur_sp USING vl_nss                , # nss
                       vl_marca_ent          , # marca entrant
                       vl_consecutivo        , # correlativo
                       reg_20.estado_marca   , # estado_marca
                       reg_20.codigo_rechazo , # codigo rechazo
                       reg_20.marca_causa    , # marca_causa
                       reg_20.fecha_causa    , # fecha_causa
                       gc_usuario
  FETCH   cur_sp INTO vl_marca_res   ,      # misma marca si convive o
                      vl_rechazo_cod        # marca activa que rechaza
  CLOSE   cur_sp

  RETURN  vl_marca_res,
          vl_rechazo_cod
END FUNCTION


