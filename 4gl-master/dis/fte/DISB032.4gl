##############################################################################
#Proyecto            => SAFRE
#Owner               => E.F.P.
#Programa            => Genera saldo de las subcuentas 1,2,3,5,6,7,9
#Fecha               => 21 mayo 2002. 13:00
#By                  => GERAROD ALFONSO VEGA PAREDES
#Sistema             => DIS
##############################################################################

DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
       nss          CHAR(11)
   END RECORD

   DEFINE l_reg RECORD LIKE dis_cuenta.* 

   DEFINE vsaldo_pesos    DECIMAL(16,6),
          vsaldo_acciones DECIMAL(16,6),
          vsubcuenta      SMALLINT

   DEFINE fecha_corte DATE,
	  hoy         DATE,
	  ejecuta     CHAR(400),
	  opc         CHAR(01)

END GLOBALS

MAIN

   CALL principal()

END MAIN

FUNCTION principal()

   CALL limpia_tabla()

   ERROR "GENERANDO INFORMACION ..."

   LET fecha_corte = "05/31/2002"
    
   DECLARE c_cur CURSOR FOR 
   SELECT nss
   FROM   cta_ctr_cuenta
   ORDER  BY nss

   LET vsaldo_pesos = 0
   LET vsaldo_acciones = 0

   FOREACH c_cur INTO g_reg.*

      DECLARE cur_saldo CURSOR FOR
       SELECT subcuenta,
	      sum(monto_en_pesos),
              sum(monto_en_acciones)
       INTO   vsubcuenta,
	      vsaldo_pesos,
	      vsaldo_acciones
       FROM   dis_cuenta
       WHERE  nss               = g_reg.nss
       AND    subcuenta         in (1,2,3,5,6,7,9,10)
       AND    tipo_movimiento   > 0
       AND    fecha_conversion <=  fecha_corte
       GROUP  BY 1

       FOREACH cur_saldo INTO vsubcuenta,vsaldo_pesos,vsaldo_acciones
	  IF vsubcuenta IS NOT NULL THEN
             CALL Ingresa_saldo()	     
          ELSE
	     LET vsaldo_pesos = 0
	     LET vsaldo_acciones = 0
          END IF
       END FOREACH
    
   END FOREACH

END FUNCTION

FUNCTION Ingresa_saldo()
   LET l_reg.tipo_movimiento   = 888
   LET l_reg.subcuenta         = vsubcuenta
   LET l_reg.siefore           = 0
   LET l_reg.folio             = 88888888
   LET l_reg.consecutivo_lote  = 0
   LET l_reg.nss               = g_reg.nss
   LET l_reg.curp              = NULL
   LET l_reg.folio_sua         = 0
   LET l_reg.fecha_pago        = fecha_corte
   LET l_reg.fecha_valor       = fecha_corte
   LET l_reg.fecha_conversion  = fecha_corte
   LET l_reg.monto_en_pesos    = vsaldo_pesos
   LET l_reg.monto_en_acciones = vsaldo_acciones
   LET l_reg.precio_accion     = 0
   LET l_reg.dias_cotizados    = 0
   LET l_reg.sucursal          = ""
   LET l_reg.id_aportante      = "SALDO NO VIV"
   LET l_reg.estado            = 5
   LET l_reg.fecha_proceso     = TODAY
   LET l_reg.usuario           = "efp0"
   LET l_reg.fecha_archivo     = TODAY
   LET l_reg.etiqueta          = 0
   INSERT into safre_tmp:cta_saldo values (l_reg.*)
END FUNCTION

FUNCTION Limpia_tabla()

   DATABASE safre_tmp
   DROP TABLE cta_saldo;

   CREATE TABLE "safre".cta_saldo 
     (
       tipo_movimiento    smallint not null ,
       subcuenta          smallint not null ,
       siefore            smallint,
       folio              integer not null ,
       consecutivo_lote   integer,
       nss                char(11) not null ,
       curp               char(18),
       folio_sua          char(6),
       fecha_pago         date,
       fecha_valor        date,
       fecha_conversion   date,
       monto_en_pesos     decimal(22,6),
       monto_en_acciones  decimal(22,6),
       precio_accion      decimal(22,6),
       dias_cotizados     smallint,
       sucursal           char(10),
       id_aportante       char(11),
       estado             smallint,
       fecha_proceso      date,
       usuario            char(8),
       fecha_archivo      date,
       etiqueta           smallint
     ) in tmp_dbs2  extent size 1500000 next size 150000 lock mode page;
   revoke all on "safre".cta_saldo from "public";
   
   LET ejecuta ="dbaccess safre_tmp crea_idx_cta_saldo.sql" CLIPPED
   RUN ejecuta
   
   DATABASE safre_af   
   
END FUNCTION
